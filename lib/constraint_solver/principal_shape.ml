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

let create_var ~id_source () = Type.Var.create ~id_source ~name:"Principal_shape.Var" ()

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

include Comparable.Make (Self)
