open! Import
module Self = Types.Principal_shape

(* TODO: Have optimized type for solver which includes a hash for poly_shapes. 
         This will provide a cheap equality function for unification. *)

module Poly = struct
  type t = Self.Poly.t =
    { quantifiers : Type.Var.t list
    ; scheme : Type.Scheme.t
    }
  [@@deriving sexp, equal, compare, hash]

  let create_unchecked ?(quantifiers = []) scheme = { quantifiers; scheme }

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let shape_quantifiers = Type.Var.Hash_set.of_list t.quantifiers in
      let scheme_quantifiers = Type.Var.Hash_set.of_list t.scheme.quantifiers in
      let num_quantifiers =
        Hash_set.length shape_quantifiers + Hash_set.length scheme_quantifiers
      in
      (* Invariant: all quantifiers are canonical. They are named using [0, num_quantifiers).  *)
      let check_canonical_names () =
        let marks = Array.create ~len:num_quantifiers false in
        let check_canonical_name (var : Type.Var.t) =
          assert (String.(var.name = "Principal_shape.Var"));
          let idx = (var.id :> int) in
          assert (not marks.(idx));
          marks.(idx) <- true
        in
        List.iter t.quantifiers ~f:check_canonical_name;
        List.iter t.scheme.quantifiers ~f:check_canonical_name;
        (* All variables are within [0, num_quantifiers). No duplicates *)
        assert (Array.for_all marks ~f:Fun.id)
      in
      let check_scheme_body () =
        let uses = Array.create ~len:num_quantifiers 0 in
        let[@inline] mark_use (var : Type.Var.t) =
          let idx = (var.id :> int) in
          uses.(idx) <- uses.(idx) + 1
        in
        let[@inline] num_uses (var : Type.Var.t) = uses.((var.id :> int)) in
        let[@inline] is_polyshape (shape : Self.t) =
          match shape with
          | Sh_arrow | Sh_tuple _ | Sh_constr _ -> false
          | Sh_poly _ -> true
        in
        (* We rely on non-short-circuting operators (since marking uses is effectful) *)
        let ( ||| ) a b = a || b in
        let non_short_circuiting_exists xs ~f =
          List.fold xs ~init:false ~f:(fun acc x -> acc ||| f x)
        in
        (* Traverses the type checking invariants. Returns [true] if it contains a polymorphic part. *)
        let rec loop (type_ : Type.t) : bool =
          match type_ with
          | Var var ->
            mark_use var;
            Hash_set.mem scheme_quantifiers var
          | Arrow (type1, type2) ->
            (* Invariant: all constructors must contain a polymorphic subterm. *)
            assert (loop type1 ||| loop type2);
            true
          | Tuple types | Constr (types, _) ->
            (* Invariant: ditto. *)
            assert (non_short_circuiting_exists types ~f:loop);
            true
          | Poly _ ->
            (* Invariant: no occurrences of [Poly]. *)
            assert false
          | Shape (types, shape) ->
            (* Invariant: all shapes applications must contain a polymorphic subterm. *)
            assert (non_short_circuiting_exists types ~f:loop);
            (* Invariant: all shapes must be polyshapes. *)
            assert (is_polyshape shape);
            true
        in
        (* Invariant: body is skeletal *)
        ignore (loop t.scheme.body : bool);
        (* Invariant: shape quantifiers are linear (i.e. used exactly once). *)
        Hash_set.iter shape_quantifiers ~f:(fun var -> assert (num_uses var = 1));
        (* Invariant: all scheme quantifiers are used at least once. *)
        Hash_set.iter scheme_quantifiers ~f:(fun var -> assert (num_uses var >= 1))
      in
      check_canonical_names ();
      check_scheme_body ())
  ;;

  let create ?quantifiers scheme =
    let t = create_unchecked ?quantifiers scheme in
    invariant t;
    t
  ;;
end

type t = Self.t =
  | Sh_arrow
  | Sh_tuple of int
  | Sh_constr of int * Type.Ident.t
  | Sh_poly of Poly.t
[@@deriving sexp, equal, compare, hash]

let ( @-> ) = Sh_arrow
let constr ~arity constr = Sh_constr (arity, constr)

let tuple n =
  if n < 2 then raise (Invalid_argument "Principal_shape.tuple: expect arity to be >= 2");
  Sh_tuple n
;;

let create_var ~id_source () = Type.Var.create ~id_source ~name:"Principal_shape.Var" ()

let arity t =
  match t with
  | Sh_arrow -> 2
  | Sh_tuple n | Sh_constr (n, _) -> n
  | Sh_poly poly_shape -> List.length poly_shape.quantifiers
;;

let quantifiers t =
  match t with
  | Sh_arrow ->
    (* Invariant: All quantifiers are locally named from 0 to n *)
    let id_source = Identifier.create_source () in
    (* [let]s are used to force the correct ordering *)
    let var1 = create_var ~id_source () in
    let var2 = create_var ~id_source () in
    [ var1; var2 ]
  | Sh_tuple n | Sh_constr (n, _) ->
    (* Invariant: All quantifiers are locally named from 0 to n *)
    let id_source = Identifier.create_source () in
    List.init n ~f:(fun _ -> create_var ~id_source ())
  | Sh_poly poly_shape -> poly_shape.quantifiers
;;

module Poly_shape_decomposition = struct
  module State = struct
    type t =
      { id_source : (Identifier.source[@sexp.opaque])
      ; shape_decomposition : Type.t Type.Var.Table.t
      ; scheme_variable_renaming : Type.Var.t Type.Var.Table.t
      ; scheme_quantifiers : Type.Var.Set.t
      }
    [@@deriving sexp]

    let create scheme_quantifiers =
      { shape_decomposition = Type.Var.Table.create ()
      ; scheme_variable_renaming = Type.Var.Table.create ()
      ; id_source = Identifier.create_source ()
      ; scheme_quantifiers
      }
    ;;

    let alloc_var t = create_var ~id_source:t.id_source ()

    let rename_scheme_var t var =
      Hashtbl.find_or_add t.scheme_variable_renaming var ~default:(fun () -> alloc_var t)
    ;;

    let alloc_shape_decomposition t type_ =
      let var = alloc_var t in
      Hashtbl.set t.shape_decomposition ~key:var ~data:type_;
      var
    ;;
  end

  module Polymorphic_or_monomorphic_part = struct
    type t =
      | Polymorphic of { principal_part : Type.t }
      (** [Polymorphic { principal_part }] indicates that original type is a polymorphic body part. 
          [principal_part] is the transformed part.  *)
      | Monomorphic
      (** [Monomorphic] indicates that the original type is a monomorphic body part. *)
    [@@deriving sexp]

    let is_monomorphic t =
      match t with
      | Polymorphic _ -> false
      | Monomorphic -> true
    ;;

    module With_original = struct
      type nonrec t =
        { result : t
        ; original : Type.t
        }
      [@@deriving sexp]

      let principal_part t ~state =
        match t.result with
        | Polymorphic { principal_part } -> principal_part
        | Monomorphic -> Var (State.alloc_shape_decomposition state t.original)
      ;;

      let[@inline] is_monomorphic t = is_monomorphic t.result
    end

    let map_principal_part2 (t1 : With_original.t) (t2 : With_original.t) ~f ~state =
      match t1.result, t2.result with
      | Monomorphic, Monomorphic -> Monomorphic
      | _, _ ->
        let type1 = With_original.principal_part t1 ~state in
        let type2 = With_original.principal_part t2 ~state in
        Polymorphic { principal_part = f type1 type2 }
    ;;

    let map_principal_parts (ts : With_original.t list) ~f ~state =
      if List.for_all ts ~f:With_original.is_monomorphic
      then Monomorphic
      else (
        let types = List.map ts ~f:(With_original.principal_part ~state) in
        Polymorphic { principal_part = f types })
    ;;
  end

  let of_applied_shape (shape : Self.t) types : Type.t =
    (* All applied shapes are normalized (aside from polytypes) *)
    match shape with
    | Sh_tuple _ -> Tuple types
    | Sh_constr (_, constr) -> Constr (types, constr)
    | Sh_arrow ->
      (match types with
       | [ type1; type2 ] -> Arrow (type1, type2)
       | _ -> assert false)
    | Sh_poly _ -> Shape (types, shape)
  ;;

  let rec of_scheme_body ~(state : State.t) (type_ : Type.t)
    : Polymorphic_or_monomorphic_part.t
    =
    let module Part = Polymorphic_or_monomorphic_part in
    let self = of_scheme_body ~state in
    let self_with_original type_ : Part.With_original.t =
      { result = self type_; original = type_ }
    in
    match type_ with
    | Var v ->
      if Set.mem state.scheme_quantifiers v
      then Polymorphic { principal_part = Var (State.rename_scheme_var state v) }
      else Monomorphic
    | Arrow (t1, t2) ->
      Part.map_principal_part2
        (self_with_original t1)
        (self_with_original t2)
        ~state
        ~f:(fun t1 t2 -> Arrow (t1, t2))
    | Tuple ts ->
      Part.map_principal_parts (List.map ts ~f:self_with_original) ~state ~f:(fun ts ->
        Tuple ts)
    | Constr (ts, constr) ->
      Part.map_principal_parts (List.map ts ~f:self_with_original) ~state ~f:(fun ts ->
        Constr (ts, constr))
    | Shape (ts, shape) ->
      Part.map_principal_parts (List.map ts ~f:self_with_original) ~state ~f:(fun ts ->
        of_applied_shape shape ts)
    | Poly scm ->
      let ts, poly_shape = of_scheme scm in
      Part.map_principal_parts (List.map ts ~f:self_with_original) ~state ~f:(fun ts ->
        Shape (ts, Sh_poly poly_shape))

  and of_scheme ({ quantifiers; body } : Type.Scheme.t) : Type.t list * Poly.t =
    let state = State.create (Type.Var.Set.of_list quantifiers) in
    let scheme_body =
      let result = of_scheme_body ~state body in
      Polymorphic_or_monomorphic_part.With_original.principal_part
        { result; original = body }
        ~state
    in
    let shape_quantifiers, types =
      Hashtbl.to_alist state.shape_decomposition
      |> List.sort ~compare:(Comparable.lift Type.Var.compare ~f:fst)
      |> List.unzip
    in
    let scheme_quantifiers =
      Hashtbl.data state.scheme_variable_renaming |> List.sort ~compare:Type.Var.compare
    in
    ( types
    , (* Safety should be guaranteed by construction *)
      (Poly.create_unchecked
         ~quantifiers:shape_quantifiers
         (Type.Scheme.create ~quantifiers:scheme_quantifiers scheme_body)
       [@alert "-unsafe"]) )
  ;;
end

let poly_shape_decomposition_of_scheme = Poly_shape_decomposition.of_scheme

let poly type_scheme =
  let _, poly = poly_shape_decomposition_of_scheme type_scheme in
  Sh_poly poly
;;

module Var = struct
  module Region0 = struct
    (** For the purpose of defaulting, each shape variable belongs to a 'region', 
        which indicates where the shape variable is existentially bound. *)
    type 'a t = { mutable shape_vars : 'a list } [@@deriving sexp_of]
  end

  module Handler = struct
    type nonrec t =
      { run : t -> unit
      ; default : unit -> unit
      ; error : unit -> Omniml_error.t
      }
    [@@deriving sexp_of]

    let schedule_job job = Scheduler.(enqueue (t ())) job
    let schedule t shape = schedule_job (fun () -> t.run shape)

    let schedule_all ts shape =
      let scheduler = Scheduler.t () in
      List.iter ts ~f:(fun t ->
        let job = fun () -> t.run shape in
        Scheduler.enqueue scheduler job)
    ;;

    let schedule_default_all ts =
      [%log.global.debug "Scheduling defaults"];
      let scheduler = Scheduler.t () in
      List.iter ts ~f:(fun t ->
        [%log.global.debug "Scheduling default"];
        Scheduler.enqueue scheduler t.default)
    ;;

    let errors ts = List.map ts ~f:(fun t -> t.error ())
  end

  module S = struct
    type desc =
      | Empty of Handler.t list
      | Full of t
    [@@deriving sexp_of]

    type 'a t =
      { id : Identifier.t
      ; region : 'a Region0.t Tree.With_dirty.Node.opaque_t
      ; guards : Guard_set.t
      ; desc : desc
      }
    [@@deriving sexp_of]

    type 'a ctx = { mark_region : 'a Region0.t Tree.With_dirty.Node.t -> unit }

    exception Cannot_merge

    let merge_desc ~ctx:_ ~create:_ ~unify:_ ~type1:_ ~type2:_ t1 t2 =
      match t1, t2 with
      | Empty hs1, Empty hs2 -> Empty (hs1 @ hs2)
      | Full s1, Full s2 -> if equal s1 s2 then Full s1 else raise Cannot_merge
      | Empty hs, Full s | Full s, Empty hs ->
        Handler.schedule_all hs s;
        Full s
    ;;

    let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
      ctx.mark_region t1.region;
      ctx.mark_region t2.region;
      { id = t1.id
      ; region = Tree.nearest_common_ancestor t1.region t2.region
      ; guards = Guard_set.union t1.guards t2.guards
      ; desc = merge_desc ~ctx ~create ~unify ~type1 ~type2 t1.desc t2.desc
      }
    ;;

    let create ~id_source ~region =
      { id = Identifier.create id_source
      ; region
      ; guards = Guard_set.empty
      ; desc = Empty []
      }
    ;;
  end

  module U = Unifier.Make (S)
  include U.Term
  include U.Make_unify (S)

  let region t = (structure t).region
  let desc t = (structure t).desc

  let set_desc t desc =
    let structure = structure t in
    set_structure t { structure with desc }
  ;;

  let id t = (structure t).id

  let is_empty t =
    match desc t with
    | Empty _ -> true
    | Full _ -> false
  ;;

  let add_handler t handler =
    match desc t with
    | Full s -> Handler.schedule handler s
    | Empty handlers -> set_desc t (Empty (handler :: handlers))
  ;;

  exception Empty

  let peek_exn t =
    match desc t with
    | Empty _ -> raise Empty
    | Full s -> s
  ;;

  let errors t =
    match desc t with
    | Full _ -> []
    | Empty hs -> Handler.errors hs
  ;;

  module Region = struct
    type nonrec t = t Region0.t [@@deriving sexp_of]

    let create () : t = { shape_vars = [] }
    let register_shape_var (t : t) shape_var = t.shape_vars <- shape_var :: t.shape_vars

    module Tree = struct
      type node = t Tree.With_dirty.Node.t [@@deriving sexp_of]
      type nonrec t = t Tree.With_dirty.t [@@deriving sexp_of]

      let region (t : node) = Tree.With_dirty.Node.value t
    end

    let is_empty (t : Tree.node) = List.is_empty (Tree.region t).shape_vars
  end

  module State = struct
    type t =
      { region_tree : Region.Tree.t
      ; partially_generalized : (Identifier.t, Region.Tree.node) Hashtbl.t
      }
    [@@deriving sexp_of]

    let create ~id_source =
      { region_tree = Tree.With_dirty.create ~id_source (Region.create ())
      ; partially_generalized = Hashtbl.create (module Identifier)
      }
    ;;

    let root_region t = Tree.With_dirty.root t.region_tree
    let is_quiet t = Tree.With_dirty.is_empty t.region_tree
    let num_partially_generalized_regions t = Hashtbl.length t.partially_generalized

    let remaining t =
      t.partially_generalized
      |> Hashtbl.data
      |> List.concat_map ~f:(fun rn -> (Region.Tree.region rn).shape_vars)
    ;;
  end

  let mark_region ~(state : State.t) node =
    Tree.With_dirty.mark_dirty state.region_tree node
  ;;

  exception Not_empty

  let fill_exn ~state t shape =
    match desc t with
    | Full shape' -> if not (equal shape shape') then raise Not_empty
    | Empty hs ->
      mark_region ~state (region t);
      Handler.schedule_all hs shape;
      set_desc t (Full shape)
  ;;

  let register_shape_var ~(state : State.t) node t =
    mark_region ~state node;
    Region.register_shape_var (Region.Tree.region node) t
  ;;

  let unsafe_set_region_if_ancestor ~state t rn =
    let structure = structure t in
    let rn' = structure.region in
    if Tree.compare_node_by_level rn rn' < 0
    then (
      mark_region ~state rn';
      set_structure t { structure with region = rn })
  ;;

  let unify ~(state : State.t) =
    unify
      ~ctx:{ mark_region = (fun rn -> Tree.With_dirty.mark_dirty state.region_tree rn) }
  ;;

  let create ~id_source ~state ~region =
    let t = create (S.create ~id_source ~region) in
    register_shape_var ~state region t;
    t
  ;;

  module Guard = Guard_set.Transitive_guard

  let is_unguarded t = Guard_set.is_empty (structure t).guards

  let add_guard ~state t g =
    let structure = structure t in
    mark_region ~state structure.region;
    set_structure
      t
      { structure with guards = Guard_set.add_transitive_guard structure.guards g }
  ;;

  let remove_guard ~state t g =
    let structure = structure t in
    mark_region ~state structure.region;
    set_structure
      t
      { structure with guards = Guard_set.remove_transitive_guard structure.guards g }
  ;;

  let clear_guard ~state t g =
    let structure = structure t in
    mark_region ~state structure.region;
    set_structure
      t
      { structure with guards = Guard_set.clear_transitive_guard structure.guards g }
  ;;

  let collect_and_rehome_a_region ~(state : State.t) (rn : Region.Tree.node) ~collect =
    let t = Region.Tree.region rn in
    let shape_vars = t.shape_vars in
    let shape_vars =
      List.filter shape_vars ~f:(fun shape_var ->
        is_representative shape_var
        && is_empty shape_var
        &&
        let rn' = region shape_var in
        if Identifier.(rn.id <> rn'.id)
        then (
          register_shape_var ~state rn' shape_var;
          false)
        else true)
    in
    let unguarded, guarded = List.partition_tf shape_vars ~f:is_unguarded in
    t.shape_vars <- guarded;
    List.iter unguarded ~f:(fun shape_var -> collect shape_var);
    if List.is_empty guarded
    then Hashtbl.remove state.partially_generalized rn.id
    else Hashtbl.set state.partially_generalized ~key:rn.id ~data:rn
  ;;

  let default_on_collect shape_var =
    match desc shape_var with
    | Full _ -> assert false
    | Empty hs -> Handler.schedule_default_all hs
  ;;

  let error_on_collect () =
    let errors = ref [] in
    let on_collect shape_var =
      match desc shape_var with
      | Full _ -> assert false
      | Empty hs -> errors := Handler.errors hs @ !errors
    in
    (fun () -> !errors), on_collect
  ;;

  let collect_rehome_and_default ~(state : State.t) rn =
    let noop = Fn.id in
    Tree.With_dirty.drain_dirty
      state.region_tree
      rn
      ~before:noop
      ~after:noop
      ~f:(collect_and_rehome_a_region ~state ~collect:default_on_collect)
  ;;

  let collect_rehome_and_default_roots ~(state : State.t) =
    let noop = Fn.id in
    Tree.With_dirty.drain_dirty_roots
      state.region_tree
      ~before:noop
      ~after:noop
      ~f:(collect_and_rehome_a_region ~state ~collect:default_on_collect)
  ;;

  let collect_rehome_and_error ~(state : State.t) rn =
    let noop = Fn.id in
    let errors, collect = error_on_collect () in
    Tree.With_dirty.drain_dirty
      state.region_tree
      rn
      ~before:noop
      ~after:noop
      ~f:(collect_and_rehome_a_region ~state ~collect);
    errors ()
  ;;

  let collect_rehome_and_error_roots ~(state : State.t) =
    let noop = Fn.id in
    let errors, collect = error_on_collect () in
    Tree.With_dirty.drain_dirty_roots
      state.region_tree
      ~before:noop
      ~after:noop
      ~f:(collect_and_rehome_a_region ~state ~collect);
    errors ()
  ;;
end

include Comparable.Make (Self)
