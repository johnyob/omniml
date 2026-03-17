open! Import
module F = Structure.Former
module M = Structure.Shape_var (F)
module R = Structure.Rigid (M)
module I = Structure.First_order (R)

module Pool = struct
  type 'a t =
    { mutable types : 'a list
    ; mutable rigid_vars : 'a list
    ; mutable shape_var_region : Principal_shape.Var.Region.Tree.node option
    ; parent_shape_var_region : Principal_shape.Var.Region.Tree.node
    ; raise_scope_escape : 'a -> unit
    }
  [@@deriving sexp_of]

  let create ~raise_scope_escape ~parent_shape_var_region () =
    { raise_scope_escape
    ; parent_shape_var_region
    ; shape_var_region = None
    ; types = []
    ; rigid_vars = []
    }
  ;;

  let register_type t type_ = t.types <- type_ :: t.types
  let register_rigid_var t rigid_var = t.rigid_vars <- rigid_var :: t.rigid_vars
  let is_dead t = List.is_empty t.types
  let is_alive t = not (is_dead t)
end

module Region0 = struct
  (** The [Generalization] module manages the generalisation of graphical types.

      Each type belongs to a 'region', which indicates where those types are
      existentially bound in the solver's stack. *)
  type 'a t = 'a Pool.t Tree.With_dirty.Node.t [@@deriving sexp_of]

  (** A type used to trick ppx_sexp_conv *)
  type 'a opaque_t = 'a Pool.t Tree.With_dirty.Node.opaque_t [@@deriving sexp_of]

  let pool t = Tree.With_dirty.Node.value t
end

(** An instance identifier is a uid allocated for each instance *)
module Instance_identifier = Identifier

module Status = struct
  type t =
    | Instance of { dirty : bool }
    (** [Instance { dirty }] indicates that the term is not 
        fully generalized. If [dirty] is true, then the term's  
        *owning* region has been marked, indicating that the term 
        has updates that need propagating. 

        TODO: It would be nice if we could track the dirtiness 
        of each type individually. This would result in more 
        efficient generalization since guards use a reference 
        count (computed by a greatest fixed point). So we 
        could in thoery do this! *)
    | Generic
    (** [Generic] indicates that the term is fully generalized 
        and belongs to no region. Once a term is generalized, it is 
        removed from it's pool. *)
  [@@deriving sexp_of]

  let is_dirty t =
    match t with
    | Instance { dirty } -> dirty
    | Generic ->
      raise_bug_s ~here:[%here] [%message "Status.is_dirty Generic is undefined"]
  ;;

  let merge t1 t2 =
    match t1, t2 with
    | Generic, _ | _, Generic ->
      raise_bug_s ~here:[%here] [%message "Cannot merge Generic nodes" (t1 : t) (t2 : t)]
    | Instance _, Instance _ ->
      (* Any update should mark the status as dirty. *)
      Instance { dirty = true }
  ;;

  let is_generic t =
    match t with
    | Generic -> true
    | Instance _ -> false
  ;;
end

module S = struct
  type 'a t =
    { id : Identifier.t
    ; inner : 'a I.t
    ; guards : Guard_set.t
    ; status : Status.t
    ; region : 'a Region0.opaque_t
    ; instances : 'a instances
    }

  and 'a instances = ('a Region0.opaque_t * 'a) Instance_identifier.Map.t
  [@@deriving sexp_of]

  let create ~id_source ~region inner =
    { id = Identifier.create id_source
    ; status = Instance { dirty = true }
    ; guards = Guard_set.empty
    ; region
    ; instances = Instance_identifier.Map.empty
    ; inner
    }
  ;;

  type 'a ctx =
    { id_source : Identifier.source
    ; curr_region : 'a Region0.t
    ; mark_region : 'a Region0.t -> unit
    ; remove_guard : 'a -> unit
    ; shape_var_state : Principal_shape.Var.State.t
    ; prune_structure : 'a M.t -> 'a instances -> unit
    }

  let decomposition_of_structure = fun { F.args; shape } -> args, shape

  let super_ctx ctx =
    { M.super = (); shape_var_state = ctx.shape_var_state; decomposition_of_structure }
  ;;

  exception Cannot_merge = I.Cannot_merge

  let iter t ~f = I.iter t.inner ~f
  let fold t ~init ~f = I.fold t.inner ~init ~f
  let is_var t = I.is_var t.inner

  let lower ~(to_ : 'a Region0.t) ~remove_guard ~unify_inst t =
    [%log.global.debug
      "Lowering" (t.id : Identifier.t) (Map.key_set t.instances : Identifier.Set.t)];
    Map.filter t.instances ~f:(fun (from, inst) ->
      (* We compare [to_] and [from] by level. 
         
         Safety: It is guaranteed that [to_] and [from] 
         lie on the same path from the root => safe to 
         compare by level.

         More precisely, in the case of [merge], [to_] is 
         some LCA involving [from]. In the case of [lower] 
         (called from [generalize_generation], [to_] is 
         guaranteed to be on the same path as [from] from 
         the root due to scoping invariants of generalization. *)
      if Tree.compare_node_by_level to_ from < 0
      then (
        remove_guard inst;
        unify_inst inst;
        false)
      else true)
  ;;

  let merge ~ctx ~create:create_type ~unify ~type1 ~type2 t1 t2 =
    [%log.global.debug "Merging" (t1.id : Identifier.t) (t2.id : Identifier.t)];
    (* Perform the necessary write barriers *)
    if not (Status.is_dirty t1.status) then ctx.mark_region t1.region;
    if not (Status.is_dirty t2.status) then ctx.mark_region t2.region;
    (* Computing the nearest common ancestor is indeed necessary here.

         Consider three regions arranged like this:

             R0
            /  \
           R1  R2

         - In R0, we have the variable 'a
         - In R1, we unify 'a with 'b list
         - In R2, we unify 'a with 'c list list

         This implies that: 'b = 'c list.

         However:
         - 'c list has the status [Instance R2]
         - 'b has the status [Instance R1]

         On unifying 'b and 'c list, we must lower the rank of
         'b to [Instance R0], the nearest common ancestor of R1
         and R2. *)
    let region = Tree.nearest_common_ancestor t1.region t2.region in
    (* We now need to notify the instances of [t1] (or [t2]) if 
       they have been lowered. *)
    let unify_inst inst =
      (* It doesn't matter which type ([type1] or [type2]) we pick, since after
         the current unification is successful, [type1] and [type2] will refer 
         to the *same* type. *)
      unify type2 inst
    in
    let instances1 = lower ~to_:region ~remove_guard:ctx.remove_guard ~unify_inst t1 in
    let instances2 = lower ~to_:region ~remove_guard:ctx.remove_guard ~unify_inst t2 in
    (* Merge inner structures *)
    let create inner =
      let region = ctx.curr_region in
      let type_ = create_type (create ~id_source:ctx.id_source ~region inner) in
      ctx.mark_region region;
      Pool.register_type (Region0.pool region) type_;
      type_
    in
    let inner =
      I.merge ~ctx:(super_ctx ctx) ~create ~unify ~type1 ~type2 t1.inner t2.inner
    in
    (* We now determine if there are updates to be propagated to instances *)
    (match inner with
     | Structure (Structure structure) ->
       if is_var t1 then ctx.prune_structure structure instances1;
       if is_var t2 then ctx.prune_structure structure instances2
     | _ -> ());
    (* We now merge instances *)
    let instances =
      Map.merge_skewed
        instances1
        instances2
        ~combine:(fun ~key:iid (from1, i1) (_from2, i2) ->
          (* We need to merge two instance edges [i('a) ~> 'c] and 
             [i('a) ~> 'd] by unifying ['c] and ['d]. 

             The resulting instance must avoid double counting the guards. *)
          [%log.global.debug "Combining instances" (iid : Instance_identifier.t)];
          (* Safety: all instances are directly guarded *)
          ctx.remove_guard i2;
          unify i1 i2;
          (* Invariant: [from1 = from2] *)
          from1, i1)
    in
    let status = Status.merge t1.status t2.status in
    let guards = Guard_set.union t1.guards t2.guards in
    let id =
      (* It doesn't matter which id we pick. We use the minimum (ie the oldest)
         since it helps with logging *)
      Identifier.min t1.id t2.id
    in
    { id; status; inner; guards; region; instances }
  ;;
end

module U = Unifier.Make (S)
module Type0 = U.Term

module State = struct
  type t =
    { id_source : (Identifier.source[@sexp.opaque])
    ; region_tree : Type0.t Pool.t Tree.With_dirty.t
    ; alive_regions : (Identifier.t, Type0.t Region0.t) Hashtbl.t
    ; shape_var_state : Principal_shape.Var.State.t
    ; defaulting : Omniml_options.Defaulting.t
    }
  [@@deriving sexp_of]

  let create ?(defaulting = Omniml_options.Defaulting.default) () =
    let id_source = Identifier.create_source () in
    let shape_var_state = Principal_shape.Var.State.create ~id_source in
    let root_shape_var_region = Principal_shape.Var.State.root_region shape_var_state in
    let root_pool =
      Pool.create
        ~raise_scope_escape:(fun _ ->
          raise_bug_s
            ~here:[%here]
            [%message "The root region should not bind any rigid variables"])
        ~parent_shape_var_region:root_shape_var_region
        ()
    in
    let region_tree = Tree.With_dirty.create ~id_source root_pool in
    { id_source
    ; region_tree
    ; shape_var_state
    ; alive_regions = Hashtbl.create (module Identifier)
    ; defaulting
    }
  ;;

  let root_region t = Tree.With_dirty.root t.region_tree
  let root_shape_var_region t = Principal_shape.Var.State.root_region t.shape_var_state

  let mark_region_as_alive t (region : _ Region0.t) =
    Hashtbl.set t.alive_regions ~key:region.id ~data:region
  ;;

  let mark_region_as_dead t (region : _ Region0.t) =
    Hashtbl.remove t.alive_regions region.id
  ;;

  let num_alive_regions t = Hashtbl.length t.alive_regions
end

module Region = struct
  type t = Type0.t Region0.t [@@deriving sexp_of]
  type opaque_t = Type0.t Region0.opaque_t [@@deriving sexp_of]

  let create ~(state : State.t) ~(curr_region : t) pool =
    Tree.With_dirty.create_node ~id_source:state.id_source ~parent:curr_region pool
  ;;

  let mark ~(state : State.t) t = Tree.With_dirty.mark_dirty state.region_tree t
  let pool t : Type0.t Pool.t = Region0.pool t
  let types t = (pool t).types

  let register_type ~(state : State.t) t term =
    mark ~state t;
    State.mark_region_as_alive state t;
    Pool.register_type (pool t) term
  ;;

  let register_rigid_var ~(state : State.t) t term =
    mark ~state t;
    State.mark_region_as_alive state t;
    Pool.register_rigid_var (pool t) term
  ;;
end

module Type = struct
  type t = Type0.t [@@deriving sexp_of]

  let structure = Type0.structure
  let unsafe_set_structure = Type0.set_structure
  let unsafe_update_structure t f = unsafe_set_structure t (f (structure t))
  let is_representative = Type0.is_representative
  let same_class = Type0.same_class
  let id t = (structure t).id
  let inner t = (structure t).inner
  let region t = (structure t).region
  let level t = (region t).level
  let status t = (structure t).status
  let is_dirty t = Status.is_dirty (status t)
  let is_generic t = Status.is_generic (status t)
  let is_var t = S.is_var (structure t)

  module Mark = struct
    type 'a t = (Identifier.t, 'a) Hashtbl.t [@@deriving sexp_of]

    let create () = Hashtbl.create (module Identifier)
    let mem t term = Hashtbl.mem t (id term)

    let mark t term data =
      match Hashtbl.add t ~key:(id term) ~data with
      | `Ok -> true
      | `Duplicate -> false
    ;;
  end

  let is_marked t mark = Mark.mem mark t
  let try_mark t mark data = Mark.mark mark t data

  let write_barrier ~state t =
    unsafe_update_structure t (fun structure ->
      if not (Status.is_dirty structure.status) then Region.mark ~state structure.region;
      { structure with S.status = Instance { dirty = true } })
  ;;

  let clear_write_barrier t =
    unsafe_update_structure t (fun structure ->
      { structure with S.status = Instance { dirty = false } })
  ;;

  let update_structure ~state t f =
    write_barrier ~state t;
    unsafe_update_structure t f
  ;;

  let instances t = (structure t).instances

  let add_instance ~state t iid instance =
    update_structure ~state t (fun structure ->
      { structure with
        instances = Map.add_exn structure.instances ~key:iid ~data:instance
      })
  ;;

  let guards t = (structure t).guards
  let is_unguarded t = Guard_set.is_empty (guards t)
  let is_guarded t = not (is_unguarded t)

  let is_transitively_guarded t ~(by : Region.t) =
    Guard_set.is_transitively_guarded (guards t) ~by:by.id
  ;;

  let unsafe_add_transitive_guard t (region : Region.t) =
    let structure = structure t in
    unsafe_set_structure
      t
      { structure with
        guards = Guard_set.add_transitive_guard structure.guards region.id
      }
  ;;

  let unsafe_remove_transitive_guard t (region : Region.t) =
    let structure = structure t in
    unsafe_set_structure
      t
      { structure with
        guards = Guard_set.remove_transitive_guard structure.guards region.id
      }
  ;;

  let unsafe_clear_transitive_guard t (region : Region.t) =
    let structure = structure t in
    unsafe_set_structure
      t
      { structure with
        guards = Guard_set.clear_transitive_guard structure.guards region.id
      }
  ;;

  let add_guard ~state t =
    update_structure ~state t (fun structure ->
      { structure with guards = Guard_set.add_guard structure.guards })
  ;;

  let remove_guard ~state t =
    update_structure ~state t (fun structure ->
      { structure with guards = Guard_set.remove_guard structure.guards })
  ;;

  let create ~(state : State.t) ~curr_region structure =
    let structure = S.create ~id_source:state.id_source ~region:curr_region structure in
    let t = Type0.create structure in
    Region.register_type ~state curr_region t;
    t
  ;;
end

module Scheme = struct
  type t =
    { root : Type.t
    ; region : Region.t
    }
  [@@deriving sexp_of]

  let body t = t.root

  let iter_instances_and_partial_generics t ~f =
    let visited = Hash_set.create (module Identifier) in
    let rec loop type_ =
      let id = Type.id type_ in
      if not (Hash_set.mem visited id)
      then (
        match Type.status type_ with
        | Generic -> Type.inner type_ |> I.iter ~f:loop
        | Instance _ -> f type_)
    in
    loop t.root
  ;;
end

let create_var ~state ~curr_region () = Type.create ~state ~curr_region Var

let create_shape_var ~(state : State.t) ~curr_region () =
  (* We create shape variables in the *parent* region *)
  let parent_shape_var_region = (Region.pool curr_region).parent_shape_var_region in
  let shape_var =
    Principal_shape.Var.create
      ~id_source:state.id_source
      ~state:state.shape_var_state
      ~region:parent_shape_var_region
  in
  shape_var
;;

let create_rigid_var ~state ~curr_region () =
  let rigid_var = Type.create ~state ~curr_region (Structure Rigid_var) in
  Region.register_rigid_var ~state curr_region rigid_var;
  rigid_var
;;

let create_shape_args ~state ~curr_region args =
  Type.create ~state ~curr_region (Structure (Structure (Shape_args args)))
;;

let create_shape_app ~state ~curr_region args shape_var =
  Type.create ~state ~curr_region (Structure (Structure (Shape_app { args; shape_var })))
;;

let create_former ~state ~curr_region former =
  Type.create ~state ~curr_region (Structure (Structure (Structure former)))
;;

module Unify = U.Make_unify (S)

(** [flexize_inner inner] flexizes a rigid variable into a flexible 
    unification structure. This is performed at instantiation (in the  
    case of incremental instantiation) or generalization (an optimization 
    to avoid repeated flexization) *)
let flexize_inner inner =
  match inner with
  | I.Structure Rigid_var -> I.Var
  | _ -> inner
;;

(** [prune_structure ~state ~src_region structure instances] prunes all 
    instantiation edges in [instances], pushing them to the leaves 
    of [structure]. 

    This removes the guards on each instance (but potentially creates new 
    instantation edges). This is only called on types with instances 
    whose structure has just been updated from a [Var]. *)
let rec prune_structure ~(state : State.t) structure instances =
  [%log.global.debug
    "Pruning structure"
      (structure : Type.t M.t)
      (instances : (Region.t * Type.t) Instance_identifier.Map.t)];
  Map.iteri instances ~f:(fun ~key:instance_id ~data:(from, instance) ->
    [%log.global.debug
      "Pruning instance" (instance_id : Instance_identifier.t) (instance : Type.t)];
    let dst_region = Type.region instance in
    let copy =
      Type.create
        ~state
        ~curr_region:dst_region
        (Structure
           (Structure
              (structure
               |> M.map ~f:(copy ~state ~instance_id ~src_region:from ~dst_region))))
    in
    [%log.global.debug "Copy" (copy : Type.t)];
    unify ~state ~curr_region:dst_region copy instance)

(** [copy ~state type_ ~instance_id ~src_region ~dst_region] copies the type [type_] 
    in the [src_region] region to a fresh type in the [dst_region] region. Any 
    instantiation edges are associated with the instance id [instance_id].  *)
and copy ~(state : State.t) type_ ~instance_id ~src_region ~dst_region =
  let generic_copies = Hashtbl.create (module Identifier) in
  let rec visit type_ =
    if Tree.compare_node_by_level (Type.region type_) src_region < 0
    then type_
    else (
      match Type.status type_ with
      | Generic -> find_or_alloc_generic_copy type_
      | Instance _ ->
        (* The type is an instance, but a member of the region. Meaning it 
           could be generic in the future. So we create an instantiation 
           edge. *)
        find_or_alloc_instance_copy type_)
  and alloc_copy ~on_alloc type_ =
    let copy = create_var ~state ~curr_region:dst_region () in
    on_alloc copy;
    let inner = Type.inner type_ in
    let flexized_inner = if Type.is_generic type_ then inner else flexize_inner inner in
    let inner_copy = I.map flexized_inner ~f:visit in
    unify
      ~state
      ~curr_region:dst_region
      copy
      (Type.create ~state ~curr_region:dst_region inner_copy);
    copy
  and find_or_alloc_generic_copy type_ =
    let id = Type.id type_ in
    try Hashtbl.find_exn generic_copies id with
    | Not_found_s _ ->
      alloc_copy
        ~on_alloc:(fun copy -> Hashtbl.set generic_copies ~key:id ~data:copy)
        type_
  and find_or_alloc_instance_copy type_ =
    try
      let _from, instance = Map.find_exn (Type.instances type_) instance_id in
      (* Invariant: [from = src_region] *)
      instance
    with
    | Not_found_s _ ->
      alloc_copy
        ~on_alloc:(fun instance ->
          [%log.global.debug "Adding instance guard" (type_ : Type.t) (instance : Type.t)];
          Type.add_guard ~state instance;
          Type.add_instance ~state type_ instance_id (src_region, instance))
        type_
  in
  visit type_

and unify ~(state : State.t) ~curr_region type1 type2 =
  let remove_guard_worklist = Queue.create () in
  let prune_worklist = Queue.create () in
  let unifier_ctx : _ S.ctx =
    { id_source = state.id_source
    ; curr_region
    ; mark_region = Region.mark ~state
    ; remove_guard = Queue.enqueue remove_guard_worklist
    ; shape_var_state = state.shape_var_state
    ; prune_structure =
        (fun structure instances -> Queue.enqueue prune_worklist (structure, instances))
    }
  in
  Unify.try_unify_or_rollback ~ctx:unifier_ctx type1 type2;
  Queue.iter remove_guard_worklist ~f:(Type.remove_guard ~state);
  Queue.iter prune_worklist ~f:(fun (structure, instances) ->
    prune_structure ~state structure instances)
;;

module Generation = struct
  type t =
    { region : Region.t
    ; type_ids : Identifier.t Hash_set.t
    }
  [@@deriving sexp_of]

  let create region =
    let pool = Region.pool region in
    let type_ids = Hash_set.create (module Identifier) in
    pool.Pool.types |> List.iter ~f:(fun type_ -> Hash_set.add type_ids (Type.id type_));
    { region; type_ids }
  ;;

  let types t = (Region.pool t.region).types
  let is_region t (region : Region.t) = Identifier.(region.id = t.region.id)

  (** [mem t type_] returns [true] if the given [type_] belongs to the 
      generation's region. *)
  let mem t type_ = Hash_set.mem t.type_ids (Type.id type_)

  (** [mem_after_level_updates t type_] returns [true] when the given [type_] 
      still belongs to the generation's region. *)
  let mem_after_level_updates t type_ = is_region t (Type.region type_)

  let unsafe_adjust_region_by_level ~state t type_ r =
    let r' = Type.region type_ in
    if Tree.compare_node_by_level r r' < 0
    then (
      (* Adjust it's region since it is an ancestor *)
      (* We don't want to remark [generation.region] as dirty 
           via a write barrier, so we check beforehand. *)
      if not (is_region t r') then Type.write_barrier ~state type_;
      (* Safety: We've performed a write barrier (unless the term belongs 
         to the current generation, at which point [generalize_generation] 
         will clear the write barrier). *)
      Type.unsafe_update_structure type_ (fun structure -> { structure with region = r }))
  ;;
end

open State

let enter_region ~state ~raise_scope_escape curr_region =
  let shape_var_region =
    match state.defaulting with
    | Disabled ->
      (* If defaulting is disabled, then we allocate all shape 
         variables in the root region. *)
      State.root_shape_var_region state
    | Unary ->
      (* When we enter a new region, we have to potentially 
         allocate the current region's shape var region. *)
      let p = Region.pool curr_region in
      (match p.shape_var_region with
       | None ->
         let shape_var_region =
           Tree.With_dirty.create_node
             ~id_source:state.id_source
             ~parent:p.parent_shape_var_region
             (Principal_shape.Var.Region.create ())
         in
         p.shape_var_region <- Some shape_var_region;
         shape_var_region
       | Some shape_var_region -> shape_var_region)
  in
  Tree.With_dirty.create_node
    ~id_source:state.id_source
    ~parent:curr_region
    (Pool.create ~raise_scope_escape ~parent_shape_var_region:shape_var_region ())
;;

let update_regions ~state (generation : Generation.t) =
  [%log.global.debug "Updating regions" (generation : Generation.t)];
  let guarded_roots = ref [] in
  let transitively_guarded_cross_region_nodes = ref [] in
  let mark = Type.Mark.create () in
  let rec loop type_ r =
    (* Invariant: [r.level <= generation.level]

       This is guaranteed by the region invariant (scopes can only increase). *)
    [%log.global.debug "Visiting" (type_ : Type.t) (r : Region.opaque_t)];
    if Type.try_mark type_ mark ()
    then (
      [%log.global.debug "Not previously visited"];
      (* Invariant: [r] and [Type.region term] lie on a given path from 
         the root region. This is guaranteed by scoping invariants. 

         This invariant ensures that these regions can be compared by levels. *)
      Generation.unsafe_adjust_region_by_level ~state generation type_ r;
      [%log.global.debug "Adjusted region" (type_ : Type.t)];
      let was_transitively_guarded =
        Type.is_transitively_guarded type_ ~by:generation.region
      in
      (* Safety: no need to mark the type's region, see [mark_guarded] *)
      Type.unsafe_clear_transitive_guard type_ generation.region;
      [%log.global.debug "Cleared transitive guards" (type_ : Type.t)];
      if not (Generation.mem generation type_)
      then (
        [%log.global.debug "Type not in generation"];
        (* [type_] is in an ancestor region of the current generation.
           We do not need to visit it. *)
        assert (Tree.Level.((Type.region type_).level <= r.level));
        (* We mark [type_] as a old cross-region node that was transitively 
           guarded by the current generation. Clearing its region may 
           consistute a state update in its region. *)
        if was_transitively_guarded
        then
          transitively_guarded_cross_region_nodes
          := type_ :: !transitively_guarded_cross_region_nodes)
      else (
        [%log.global.debug "Type in generation, visiting children"];
        (* [type_] is owned by [generation]. *)
        (* If [type_] is guarded *and* it's region is the current generation, 
           then it is a root that should be used to trace guards. *)
        if Type.is_guarded type_ && Generation.mem_after_level_updates generation type_
        then guarded_roots := type_ :: !guarded_roots;
        (* Recurse to any child nodes of [type_] *)
        dominate_inner (Type.inner type_) r))
    else (
      (* We've previously visited [type_]. Since we visit types in order of 
         domination (sorting types by level to begin with), then it 
         follows that [type_]'s level is lower (older) than [r]'s level *)
      [%log.global.debug "Already visited"];
      assert (Tree.Level.((Type.region type_).level <= r.level)))
  and dominate_inner (s : Type.t I.t) r =
    match s with
    | Var | Structure Rigid_var -> ()
    | Structure (Structure s) -> dominate_shape_var s r
  and dominate_shape_var (s : Type.t M.t) r =
    match s with
    | Shape_app { args; shape_var } ->
      Principal_shape.Var.clear_guard
        ~state:state.shape_var_state
        shape_var
        generation.region.id;
      (* Safety: r and svar's region must lie on a given path by scoping. 
         So comparing by levels is safe here. *)
      Principal_shape.Var.unsafe_set_region_if_ancestor
        ~state:state.shape_var_state
        shape_var
        (Region.pool r).parent_shape_var_region;
      loop args r
    | Shape_args args -> List.iter args ~f:(fun arg -> loop arg r)
    | Structure f -> F.iter f ~f:(fun t -> loop t r)
  in
  generation
  |> Generation.types
  |> List.sort ~compare:(Comparable.lift Tree.Level.compare ~f:Type.level)
  |> List.iter ~f:(fun type_ -> loop type_ (Type.region type_));
  !guarded_roots, !transitively_guarded_cross_region_nodes
;;

let mark_guarded
      ~state
      ~guarded_roots
      ~transitively_guarded_cross_region_nodes
      (generation : Generation.t)
  =
  [%log.global.debug
    "Updating type guards" (generation : Generation.t) (guarded_roots : Type.t list)];
  (* [update_regions] computes two things of interest for marking guards. 

     It firstly computes the the set of types that are guarded *and* still 
     belong to the current generation. 

     Secondly, it computes the set of types that are transitively guarded 
     and aren't owned by the current generation. This is the case of so-called 
     cross-region guards.

     Additionally, *all* (reachable) types have had their transitive guard removed. 
     This clears the mark bit (or more precisely counter).
  *)
  let rec visit type_ =
    [%log.global.debug "Marking type as guarded" (type_ : Type.t)];
    let marked = Type.is_transitively_guarded type_ ~by:generation.region in
    (* Safety: this operation is not protected by a write barrier. 
       There are two cases: 

       1. [type_] belongs to [generation] after level updates. 
          In this case, we don't trigger a write barrier since we're 
          currently generalizing [generation].

       2. [type_] belongs some to ancestor region of [generation]. 
          In which case, we do nothing. See below for reasoning. *)
    Type.unsafe_add_transitive_guard type_ generation.region;
    if not marked
    then (
      [%log.global.debug "Type was not transitively guarded"];
      (* If this is a cross-region link *)
      if Generation.mem_after_level_updates generation type_
      then (
        [%log.global.debug "Type is part of generation, visiting children"];
        visit_children type_)
      else (
        [%log.global.debug "Type is part of an ancestor region" (type_ : Type.t)];
        (* There are two cases: 

           1. The type *was* a member of the generation. In this case, it's level has 
              been lowered in [update_regions] and it's region will be marked as dirty 
              in [generalize_generation].

           2. The type isn't a member of the generation. This must be a *old* cross-region 
              link. In this case, do nothing since we have unset and reset the transitive 
              guard, resulting in a noop. 

          Both cases involve us doing nothing! *)
        ()))
  and visit_children type_ =
    match Type.inner type_ with
    | Structure (Structure (Shape_app { args; shape_var })) ->
      Principal_shape.Var.add_guard
        ~state:state.shape_var_state
        shape_var
        generation.region.id;
      visit args
    | inner -> I.iter inner ~f:visit
  in
  (* Mark all nodes reachable from the roots as guarded (transitively) by [region] *)
  List.iter guarded_roots ~f:visit_children;
  List.iter transitively_guarded_cross_region_nodes ~f:(fun type_ ->
    (* Perform a write barrier to account for the 
       [unsafe_clear_transitive_guards] in [update_regions] *)
    if not (Type.is_transitively_guarded type_ ~by:generation.region)
    then Type.write_barrier ~state type_)
;;

let rigid_scope_check (generation : Generation.t) =
  [%log.global.debug "Scope check generation" (generation : Generation.t)];
  (* Iterate over rigid variables, if the level of the rigid variable is
     less than the young region level, then the rigid variable has escaped
     it's scope *)
  let { Pool.rigid_vars; raise_scope_escape; _ } = Region.pool generation.region in
  match
    List.find rigid_vars ~f:(fun var ->
      not (Generation.mem_after_level_updates generation var))
  with
  | None -> ()
  | Some var -> raise_scope_escape var
;;

let unsafe_generalize ~state type_ =
  if Type.is_unguarded type_
  then (
    Map.iter (Type.instances type_) ~f:(fun (_from, instance) ->
      Type.remove_guard ~state instance);
    (* Safety: [type_] is being generalized, there is no 
       point setting it's write barrier (and dirty bit). *)
    Type.unsafe_update_structure type_ (fun structure ->
      { structure with
        inner = flexize_inner structure.inner
      ; status = Generic
      ; instances = Instance_identifier.Map.empty
      }))
;;

let lower ~state type_ =
  let curr_region = Type.region type_ in
  (* We must delay the unifications because we first must write 
     the [instances] map, which may result in write-write conflicts.
     We do this by delaying the updates using the scheduler. *)
  let unify_inst instance =
    Scheduler.(enqueue (t ()) (fun () -> unify ~curr_region ~state type_ instance))
  in
  (* Safety: The write barrier doesn't need triggering since the type is 
     fully updated by generalization. *)
  Type.unsafe_update_structure type_ (fun structure ->
    { structure with
      instances =
        S.lower
          structure
          ~to_:curr_region
          ~remove_guard:(Type.remove_guard ~state)
          ~unify_inst
    })
;;

let generalize_generation ~state (generation : Generation.t) =
  [%log.global.debug "Generalizing generation" (generation : Generation.t)];
  let generics =
    Region.types generation.region
    |> List.filter ~f:(fun type_ ->
      (* The type has had its updates propagated (levels and guards). 
         So we can clear its dirty bit. *)
      Type.clear_write_barrier type_;
      if Generation.mem_after_level_updates generation type_
      then (
        [%log.global.debug "Generalizing type" (type_ : Type.t)];
        unsafe_generalize ~state type_;
        [%log.global.debug "Generalizing type" (type_ : Type.t)];
        Type.is_representative type_)
      else (
        [%log.global.debug "Lowering type" (type_ : Type.t)];
        (* Register [type_] in the region [Term.region type_] *)
        Region.register_type ~state (Type.region type_) type_;
        lower ~state type_;
        [%log.global.debug "Lowering type" (type_ : Type.t)];
        (* Filter the type from the result list *)
        false))
  in
  [%log.global.debug "Generalized generics" (generics : Type.t list)];
  let guarded_generics, generics = List.partition_tf generics ~f:Type.is_guarded in
  [%log.global.debug "Guarded generics" (guarded_generics : Type.t list)];
  [%log.global.debug "Unguarded generics" (generics : Type.t list)];
  if List.is_empty guarded_generics
  then (
    [%log.global.debug "Region is dead"];
    State.mark_region_as_dead state generation.region);
  (Region.pool generation.region).types <- guarded_generics
;;

let update_and_default_shape_region ~state shape_var_region =
  Principal_shape.Var.collect_rehome_and_default
    ~state:state.shape_var_state
    shape_var_region
;;

let update_and_generalize_generation ~state generation =
  let guarded_roots, transitively_guarded_cross_region_nodes =
    update_regions ~state generation
  in
  rigid_scope_check generation;
  mark_guarded ~state ~guarded_roots ~transitively_guarded_cross_region_nodes generation;
  generalize_generation ~state generation;
  (* Optionally, try defaulting some shape variables (if enabled) *)
  match state.defaulting with
  | Disabled -> ()
  | Unary ->
    Option.iter
      (Region.pool generation.region).shape_var_region
      ~f:(update_and_default_shape_region ~state)
;;

let update_and_generalize ~state (curr_region : Region.t) =
  [%log.global.debug
    "Begin generalization"
      (curr_region.id : Identifier.t)
      (state.region_tree : Type.t Pool.t Tree.With_dirty.t)];
  assert (Scheduler.(is_empty (t ())));
  let generation = Generation.create curr_region in
  update_and_generalize_generation ~state generation;
  [%log.global.debug "End generalization" (curr_region.id : Identifier.t)]
;;

let create_scheme root region : Scheme.t = { root; region }
let exit_region ~curr_region root = create_scheme root curr_region
let run_scheduler () = Scheduler.(run (t ()))

let force_generalization ~state region =
  Tree.With_dirty.drain_dirty
    state.region_tree
    region
    ~before:run_scheduler
    ~f:(update_and_generalize ~state)
    ~after:run_scheduler;
  [%log.global.debug "Finished (forced) generalization" (region : Region.opaque_t)]
;;

let force_root_generalization_and_return_unsolved_shape_var_errors ~state =
  let generalize_roots () =
    Tree.With_dirty.drain_dirty_roots
      state.region_tree
      ~before:run_scheduler
      ~after:run_scheduler
      ~f:(update_and_generalize ~state)
  in
  let collected_errors =
    match state.defaulting with
    | Disabled ->
      generalize_roots ();
      Principal_shape.Var.collect_rehome_and_error_roots ~state:state.shape_var_state
    | Unary ->
      while
        generalize_roots ();
        not (Principal_shape.Var.State.is_quiet state.shape_var_state)
      do
        Principal_shape.Var.collect_rehome_and_default_roots ~state:state.shape_var_state
      done;
      []
  in
  let remaining_shape_vars = Principal_shape.Var.State.remaining state.shape_var_state in
  collected_errors @ List.concat_map remaining_shape_vars ~f:Principal_shape.Var.errors
;;

let instantiate ~state ~curr_region ({ root; region = src_region } : Scheme.t) =
  [%log.global.debug
    "Generalization tree @ instantiation"
      (state.region_tree : Type.t Pool.t Tree.With_dirty.t)];
  (* Generalize the region *)
  force_generalization ~state src_region;
  (* Create an instance group *)
  let instance_id = Instance_identifier.create state.id_source in
  (* Copy the type *)
  copy ~state ~src_region ~dst_region:curr_region ~instance_id root
;;

module Suspended_match = struct
  type t =
    { matchee : Type.t
    ; closure : closure
    ; case : curr_region:Region.t -> shape:Principal_shape.t -> args:Type.t list -> unit
    ; else_ : unit -> Principal_shape.t
    ; error : Constraint.Match_error.t -> Omniml_error.t
    }
  [@@deriving sexp_of]

  and closure =
    { variables : Type.t list
    ; schemes : Scheme.t list
    }
  [@@deriving sexp_of]

  let closure_add_guard ~state ~shape_args { variables; schemes } =
    Type.add_guard ~state shape_args;
    List.iter variables ~f:(Type.add_guard ~state);
    List.iter schemes ~f:(fun scheme ->
      (* TODO: We should propably force the generalization of the scheme here *)
      Scheme.iter_instances_and_partial_generics scheme ~f:(Type.add_guard ~state))
  ;;

  let closure_remove_guard ~state ~shape_args { variables; schemes } =
    Type.remove_guard ~state shape_args;
    List.iter variables ~f:(Type.remove_guard ~state);
    List.iter schemes ~f:(fun scheme ->
      Scheme.iter_instances_and_partial_generics scheme ~f:(Type.remove_guard ~state))
  ;;

  exception Cannot_match_on_rigid of Omniml_error.t
  exception Inconsistent_defaults of Omniml_error.t

  let match_or_yield ~state ~curr_region { matchee; case; closure; else_; error } =
    let get_or_alloc_matchee_args () =
      match Type.inner matchee with
      | Structure (Structure (Structure { args; _ })) -> args
      | Structure (Structure (Shape_app { args; shape_var })) ->
        (match Type.inner args with
         | Structure (Structure (Shape_args args)) -> args
         | Var ->
           [%log.global.debug "Allocating matchee args" (args : Type.t)];
           let shape = Principal_shape.Var.peek_exn shape_var in
           let arg_types =
             List.init (Principal_shape.arity shape) ~f:(fun _ ->
               create_var ~state ~curr_region ())
           in
           let args' = create_shape_args ~state ~curr_region arg_types in
           [%log.global.debug
             "Allocated matchee args" (arg_types : Type.t list) (args' : Type.t)];
           [%log.global.debug
             "Unify (get_or_alloc_matchee_args)" (args : Type.t) (args' : Type.t)];
           unify ~state ~curr_region args args';
           [%log.global.debug "Unified matchee args" (args : Type.t)];
           arg_types
         | _ ->
           raise_bug_s
             ~here:[%here]
             [%message
               "Kind mismatch when allocating args. Expected args, got type."
                 (args : Type.t)])
      | Structure (Structure (Shape_args _)) ->
        raise_bug_s
          ~here:[%here]
          [%message
            "Kind mismatch when allocating args. Expected type, got args."
              (matchee : Type.t)]
      | Structure Rigid_var | Var ->
        raise_bug_s
          ~here:[%here]
          [%message "Matchee type cannot have undetermined structure" (matchee : Type.t)]
    in
    let add_handler ~shape_args svar =
      [%log.global.debug "Adding handler" (svar : Principal_shape.Var.t)];
      Principal_shape.Var.add_handler
        svar
        { run =
            (fun shape ->
              let args = get_or_alloc_matchee_args () in
              (* Remove guard from closure *)
              closure_remove_guard ~state ~shape_args closure;
              (* Solve case *)
              case ~curr_region ~shape ~args;
              [%log.global.debug
                "Generalization tree after solving case"
                  (state.region_tree : Type.t Pool.t Tree.With_dirty.t)])
        ; default =
            (fun () ->
              [%log.global.debug
                "Default handler triggered" (svar : Principal_shape.Var.t)];
              let default_shape = else_ () in
              [%log.global.debug "Default shape" (default_shape : Principal_shape.t)];
              (try
                 Principal_shape.Var.fill_exn
                   ~state:state.shape_var_state
                   svar
                   default_shape
               with
               | Principal_shape.Var.Not_empty ->
                 let actual = Principal_shape.Var.peek_exn svar in
                 let report =
                   error (Inconsistent_default { actual; expected = default_shape })
                 in
                 raise (Inconsistent_defaults report));
              [%log.global.debug
                "Generalization tree running default handler"
                  (state.region_tree : Type.t Pool.t Tree.With_dirty.t)])
        ; error = (fun () -> error Cannot_default)
        };
      (* Add guard to closure *)
      closure_add_guard ~state ~shape_args closure
    in
    match Type.inner matchee with
    | Var ->
      let shape_var = create_shape_var ~state ~curr_region () in
      let shape_args = create_var ~state ~curr_region () in
      add_handler ~shape_args shape_var;
      unify
        ~state
        ~curr_region
        matchee
        (create_shape_app ~state ~curr_region shape_args shape_var)
    | Structure (Structure (Shape_app { args = shape_args; shape_var })) ->
      add_handler ~shape_args shape_var
    | Structure (Structure (Shape_args _)) ->
      raise_bug_s
        ~here:[%here]
        [%message
          "Kind mismatch when adding matchee handler. Expected type, got args."
            (matchee : Type.t)]
    | Structure Rigid_var -> raise (Cannot_match_on_rigid (error Matchee_is_rigid))
    | Structure (Structure (Structure { args; shape })) ->
      (* Optimisation: Immediately solve the case *)
      case ~curr_region ~shape ~args
  ;;
end
