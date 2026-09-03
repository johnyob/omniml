open Core
open Omniml_std
open Omniml_unifier
open Omniml_log

module type Structure = Omniml_collector_intf.Structure

module Node_status = struct
  type t =
    | Live of { dirty : bool }
    (** [Live { dirty }] is a node which may still be updated. If [dirty] is
        true, the node has updates which must be traced by the collector. *)
    | Dead (** A [Dead] node has been collected and is immutable. *)
  [@@deriving sexp_of]

  let is_live = function
    | Live _ -> true
    | Dead -> false
  ;;

  let is_dead t = not (is_live t)

  let is_dirty = function
    | Live { dirty } -> dirty
    | Dead -> assert false
  ;;
end

module Rootings = struct
  type t =
    { direct : int
    ; derived : int Identifier.Map.t
    }
  [@@deriving sexp_of]

  let empty = { direct = 0; derived = Identifier.Map.empty }

  let union t1 t2 =
    { direct = t1.direct + t2.direct
    ; derived = Map.merge_skewed t1.derived t2.derived ~combine:(fun ~key:_ -> ( + ))
    }
  ;;

  let is_direct_rooted t = t.direct > 0
  let is_derived_rooted t = not (Map.is_empty t.derived)
  let is_rooted t = is_direct_rooted t || is_derived_rooted t
  let add_direct t = { t with direct = t.direct + 1 }

  let remove_direct t =
    assert (t.direct > 0);
    { t with direct = t.direct - 1 }
  ;;

  let find_derived t id = Option.value (Map.find t.derived id) ~default:0
  let is_derived_rooted_by t id = find_derived t id > 0

  let add_derived t id =
    let count = find_derived t id + 1 in
    { t with derived = Map.set t.derived ~key:id ~data:count }
  ;;

  let clear_derived t id = { t with derived = Map.remove t.derived id }
end

module Make (S : Structure) = struct
  module Pool = struct
    type 'a t =
      { mutable terms : 'a list
      ; metadata : 'a S.Region_metadata.t
      }
    [@@deriving sexp_of]

    let create metadata = { metadata; terms = [] }
    let register_term t term = t.terms <- term :: t.terms
  end

  module Region0 = struct
    type 'a t = 'a Pool.t Tree.With_dirty.Node.t [@@deriving sexp_of]

    let pool t = Tree.With_dirty.Node.value t
  end

  module D = struct
    type 'a t =
      { id : Identifier.t
      ; rootings : Rootings.t
      ; status : Node_status.t
      ; structure : 'a S.t
      ; region : 'a Region0.t
      }

    (* A node's region owns a pool containing the node.  Rendering either its
       structure or region here would therefore recurse around the collector
       graph.  Detailed collector logging uses [Log.node] below. *)
    let sexp_of_t
          (type a)
          sexp_of_a
          ({ id; rootings; structure; status; region = _ } : a t)
      =
      [%sexp
        { id : Identifier.t
        ; rootings : Rootings.t
        ; status : Node_status.t
        ; structure : a S.t
        }]
    ;;

    type 'a ctx =
      { id_source : Identifier.source
      ; curr_region : 'a Region0.t
      ; mark_region : 'a Region0.t -> unit
      ; super : 'a S.ctx
      }

    exception Cannot_merge = S.Cannot_merge

    let create ~id_source ~region structure =
      { id = Identifier.create id_source
      ; rootings = Rootings.empty
      ; structure
      ; status =
          Live { dirty = true }
          (* By default, all newly created nodes are dirty.

             Why? They may have region updates to propagate to
             children in [structure]. *)
      ; region
      }
    ;;

    let merge ~ctx ~create:create_type ~unify ~type1 ~type2 t1 t2 =
      [%log.global.debug
        "Merging collector nodes" (t1.id : Identifier.t) (t2.id : Identifier.t)];
      (* Dead nodes are immutable. They must be copied before they can
         participate in unification. *)
      assert (Node_status.is_live t1.status);
      assert (Node_status.is_live t2.status);
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
           - 'c list is live in [R2]
           - 'b is live in [R1]

           On unifying 'b and 'c list, we must promote the owner of
           'b to [R0], the nearest common ancestor of R1
           and R2. *)
      let region = Tree.nearest_common_ancestor t1.region t2.region in
      [%log.global.debug
        "Selected merged region"
          (region.Tree.Node.id : Identifier.t)
          (region.level : Tree.Level.t)];
      (* Perform write barriers. See [write_barrier] below. *)
      if not (Node_status.is_dirty t1.status) then ctx.mark_region t1.region;
      if not (Node_status.is_dirty t2.status) then ctx.mark_region t2.region;
      (* [create] function that is region-aware *)
      let create s =
        let region = ctx.curr_region in
        let term = create_type (create ~id_source:ctx.id_source ~region s) in
        ctx.mark_region region;
        Pool.register_term (Region0.pool region) term;
        term
      in
      (* Merge all components of the structure *)
      let structure =
        S.merge ~ctx:ctx.super ~create ~unify ~type1 ~type2 t1.structure t2.structure
      in
      (* Merge the rooting sets *)
      let rootings = Rootings.union t1.rootings t2.rootings in
      (* [status] must be [Live] and dirty *)
      let status = Node_status.Live { dirty = true } in
      let id = Identifier.min t1.id t2.id in
      [%log.global.debug "Merged collector nodes" (id : Identifier.t)];
      { id; structure; rootings; status; region }
    ;;
  end

  module U = Unifier.Make (D)
  module Term0 = U.Term

  module State = struct
    type t =
      { id_source : (Identifier.source[@sexp.opaque])
      ; region_tree : Term0.t Pool.t Tree.With_dirty.t
      ; alive_regions : (Identifier.t, Term0.t Region0.t) Hashtbl.t
      }
    [@@deriving sexp_of]

    let create ~id_source ~root =
      let root_pool = Pool.create root in
      let region_tree = Tree.With_dirty.create ~id_source root_pool in
      { id_source; region_tree; alive_regions = Hashtbl.create (module Identifier) }
    ;;

    let root_region t = Tree.With_dirty.root t.region_tree
    let is_quiescent t = Tree.With_dirty.is_empty t.region_tree

    let mark_alive_region t (region : Term0.t Region0.t) =
      Hashtbl.set t.alive_regions ~key:region.id ~data:region
    ;;

    let mark_dead_region t (region : Term0.t Region0.t) =
      Hashtbl.remove t.alive_regions region.id
    ;;

    let alive_regions t = Hashtbl.data t.alive_regions
    let num_alive_regions t = Hashtbl.length t.alive_regions
  end

  module Region = struct
    type t = Term0.t Region0.t [@@deriving sexp_of]

    module Level = Tree.Level

    module Status = struct
      module T = struct
        type t =
          | Alive
          | Dead
        [@@deriving equal, compare, sexp]
      end

      include T
      include Comparable.Make (T)
    end

    let create ~(state : State.t) ~(parent : t) metadata =
      Tree.With_dirty.create_node
        ~id_source:state.id_source
        ~parent
        (Pool.create metadata)
    ;;

    let pool t : Term0.t Pool.t = Region0.pool t
    let mark ~(state : State.t) t = Tree.With_dirty.mark_dirty state.region_tree t
    let nodes t = (pool t).terms
    let metadata t = (pool t).metadata
    let parent t = t.Tree.Node.parent
    let status t = if List.is_empty (nodes t) then Status.Dead else Status.Alive
    let level (t : t) = t.level

    let register_term ~(state : State.t) t term =
      if Status.(status t = Dead) then State.mark_alive_region state t;
      mark ~state t;
      Pool.register_term (pool t) term
    ;;
  end

  module Node = struct
    type t = Term0.t [@@deriving sexp_of]

    let desc t = Term0.structure t
    let is_representative = Term0.is_representative
    let same_class = Term0.same_class
    let id t = (desc t).id
    let structure t = (desc t).structure
    let region t = (desc t).region
    let level t = (region t).level

    module Status = Node_status

    let status t = (desc t).status
    let is_live t = Status.is_live (status t)
    let is_dead t = Status.is_dead (status t)

    (* Safety: callers must either perform [write_barrier] first or be the
       generalization pass currently responsible for the term's owning region. *)
    let unsafe_set_desc t desc = Term0.set_structure t desc
    let unsafe_update_desc t f = unsafe_set_desc t (f (desc t))

    let write_barrier ~state t =
      unsafe_update_desc t (fun desc ->
        assert (Status.is_live desc.status);
        if not (Status.is_dirty desc.status) then Region.mark ~state desc.region;
        { desc with D.status = Live { dirty = true } })
    ;;

    let update_desc ~state t f =
      write_barrier ~state t;
      unsafe_update_desc t f
    ;;

    let update_structure ~state t ~f =
      update_desc ~state t (fun desc -> { desc with structure = f desc.structure })
    ;;

    module Unsafe = struct
      let set_structure t structure =
        unsafe_update_desc t (fun desc -> { desc with structure })
      ;;

      let promote ~(state : State.t) t ~into =
        (* Safety: [into] must be an ancestor of [desc.region]. Comparing their
           levels is therefore sufficient to determine whether promotion is needed. *)
        if Tree.compare_node_by_level into (region t) < 0
        then (
          [%log.global.debug
            "Promoting collector node"
              (id t : Identifier.t)
              ((region t).Tree.Node.id : Identifier.t)
              (into.Tree.Node.id : Identifier.t)];
          update_desc ~state t (fun desc -> { desc with region = into }))
      ;;
    end

    module Mark = struct
      type 'a t = (Identifier.t, 'a) Hashtbl.t [@@deriving sexp_of]

      let create () = Hashtbl.create (module Identifier)

      let mark t term data =
        match Hashtbl.add t ~key:(id term) ~data with
        | `Ok -> true
        | `Duplicate -> false
      ;;
    end

    let try_mark t mark data = Mark.mark mark t data

    let create ~(state : State.t) ~curr_region structure =
      let desc = D.create ~id_source:state.id_source ~region:curr_region structure in
      let t = Term0.create desc in
      Region.register_term ~state curr_region t;
      t
    ;;

    module Unify = U.Make_unify (D)

    let unifier_ctx ~(state : State.t) ~curr_region super : _ D.ctx =
      { id_source = state.id_source
      ; mark_region = Region.mark ~state
      ; curr_region
      ; super
      }
    ;;

    exception Unify = Unify.Unify

    let unify ~state ~curr_region ~ctx t1 t2 =
      Unify.unify ~ctx:(unifier_ctx ~state ~curr_region ctx) t1 t2
    ;;

    let try_unify_or_rollback ~state ~curr_region ~ctx t1 t2 =
      Unify.try_unify_or_rollback ~ctx:(unifier_ctx ~state ~curr_region ctx) t1 t2
    ;;

    let rootings t = (desc t).rootings

    module Rooting = struct
      let unsafe_update node ~f =
        unsafe_update_desc node (fun desc -> { desc with rootings = f desc.rootings })
      ;;

      let update ~state node ~f =
        update_desc ~state node (fun desc -> { desc with rootings = f desc.rootings })
      ;;

      let root ~state node =
        [%log.global.debug "Rooting collector node" (id node : Identifier.t)];
        update ~state node ~f:(fun rootings -> Rootings.add_direct rootings)
      ;;

      let unroot ~state node =
        [%log.global.debug "Unrooting collector node" (id node : Identifier.t)];
        update ~state node ~f:(fun rootings -> Rootings.remove_direct rootings)
      ;;
    end
  end

  module Log = struct
    (** Logging summaries deliberately contain no region metadata and render
        structural children as identifiers.  Printing either a full region or
        a full child node would recurse through region pools back into nodes. *)
    type region =
      { id : Identifier.t
      ; level : Region.Level.t
      ; status : Region.Status.t
      }
    [@@deriving sexp_of]

    type node =
      { id : Identifier.t
      ; region : region
      ; status : Node_status.t
      ; direct_roots : int
      ; total_roots : int
      ; structure : Sexp.t
      }
    [@@deriving sexp_of]

    let region region =
      { id = region.Tree.Node.id
      ; level = Region.level region
      ; status = Region.status region
      }
    ;;

    let node node =
      let rootings = Node.rootings node in
      { id = Node.id node
      ; region = region (Node.region node)
      ; status = Node.status node
      ; direct_roots = rootings.direct
      ; total_roots =
          rootings.direct
          + Map.fold rootings.derived ~init:0 ~f:(fun ~key:_ ~data:roots sum ->
            roots + sum)
      ; structure =
          S.sexp_of_t
            (fun child -> Identifier.sexp_of_t (Node.id child))
            (Node.structure node)
      }
    ;;
  end

  module Generation = struct
    type t =
      { region : Region.t
      ; term_ids : Identifier.t Hash_set.t
      }
    [@@deriving sexp_of]

    let create region =
      let pool = Region.pool region in
      let term_ids = Hash_set.create (module Identifier) in
      List.iter pool.Pool.terms ~f:(fun node -> Hash_set.add term_ids (Node.id node));
      { region; term_ids }
    ;;

    let is_region t (region : Region.t) = Identifier.(region.id = t.region.id)

    (** [mem t term] returns [true] if the given [term] belongs to the 
        generation's region. *)
    let mem t node = Hash_set.mem t.term_ids (Node.id node)

    (** [mem_after_level_updates t term] returns [true] when the given [term]
        still belongs to the generation's region. *)
    let mem_after_level_updates t node = is_region t (Node.region node)

    (* Safety: [r] and [Node.region node] must lie on the same root path. The
        generation traversal establishes this from the scoping invariant. *)
    let unsafe_adjust_region_by_level ~state t node r =
      let r' = Node.region node in
      if Tree.compare_node_by_level r r' < 0
      then (
        (* Adjust it's region since it is an ancestor *)
        (* We don't want to remark [generation.region] as dirty
           via a write barrier, so we check beforehand. *)
        if not (is_region t r') then Node.write_barrier ~state node;
        (* Safety: We've performed a write barrier (unless the term belongs
           to the current generation, at which point [sweep_generation]
           will clear the write barrier). *)
        let desc = Node.desc node in
        Node.unsafe_set_desc node { desc with region = r })
    ;;
  end

  let update_regions ~state (generation : Generation.t) =
    [%log.global.debug
      "Updating collector regions" (Log.region generation.region : Log.region)];
    let mark = Node.Mark.create () in
    let rec loop node r =
      (* Invariant: [r.level <= generation.level]

         This is guaranteed by the region invariant (scopes can only increase). *)
      assert (Node.is_live node);
      [%log.global.debug
        "Visiting collector node" (Log.node node : Log.node) (Log.region r : Log.region)];
      if Node.try_mark node mark ()
      then (
        [%log.global.debug "Collector node was not previously visited"];
        (* Invariant: [r] and [Term.region term] lie on a given path from
           the root region. This is guaranteed by scoping invariants.

           This invariant ensures that these regions can be compared by levels. *)
        Generation.unsafe_adjust_region_by_level ~state generation node r;
        [%log.global.debug "Adjusted collector node region" (Log.node node : Log.node)];
        if not (Generation.mem generation node)
        then (
          [%log.global.debug
            "Collector node belongs to an ancestor region" (Log.node node : Log.node)];
          assert (Region.Level.(Node.level node <= r.level)))
        else
          [%log.global.debug
            "Collector node is registered in the current region"
              (Log.node node : Log.node)];
        (* Continue through nodes already owned by an ancestor. Unification may
           have installed structure containing a descendant of [generation]
           after the ancestor node was relocated. Stopping at the ancestor
           would leave that descendant in a younger region, where it could be
           collected before the ancestor's own region is processed.

           [Node.region node] is the visibility bound for its children after
           the adjustment above; using the incoming [r] could unnecessarily
           promote children of an even older cross-region node. *)
        let r = Node.region node in
        S.iter (Node.structure node) ~f:(fun node -> loop node r))
      else (
        (* We've previously visited [node]. Since we visit nodes in order of
           domination (sorting by level to begin with), then it
           follows that [node]'s level is lower (older) than [r]'s level *)
        [%log.global.debug
          "Collector node was already visited" (Node.id node : Identifier.t)];
        assert (Region.Level.(Node.level node <= r.level)))
    in
    Region.nodes generation.region
    |> List.sort ~compare:(Comparable.lift Region.Level.compare ~f:Node.level)
    |> List.iter ~f:(fun node -> loop node (Node.region node))
  ;;

  let clear_current_derived_and_find_roots (generation : Generation.t) =
    let rooted_nodes = ref [] in
    let derived_rooted_cross_region_nodes = ref [] in
    let mark = Node.Mark.create () in
    let rec visit node =
      assert (Node.is_live node);
      if Node.try_mark node mark ()
      then (
        let was_derived_rooted =
          Rootings.is_derived_rooted_by (Node.rootings node) generation.region.id
        in
        Node.Rooting.unsafe_update node ~f:(fun rootings ->
          Rootings.clear_derived rootings generation.region.id);
        if Generation.mem_after_level_updates generation node
        then (
          (* Roots derived from younger regions remain valid while collecting
             this region and must keep the node's children alive just like a
             direct root. Only roots derived from this generation were cleared
             above. *)
          if Rootings.is_rooted (Node.rootings node)
          then rooted_nodes := node :: !rooted_nodes;
          S.iter (Node.structure node) ~f:visit)
        else
          derived_rooted_cross_region_nodes
          := (node, was_derived_rooted) :: !derived_rooted_cross_region_nodes)
    in
    Region.nodes generation.region |> List.iter ~f:visit;
    !rooted_nodes, !derived_rooted_cross_region_nodes
  ;;

  let mark_rooted ~state ~rooted_nodes ~cross_region_nodes (generation : Generation.t) =
    [%log.global.debug
      "Tracing collector roots"
        (Log.region generation.region : Log.region)
        (List.map rooted_nodes ~f:(fun node -> Node.id node) : Identifier.t list)];
    let rec visit node =
      assert (Node.is_live node);
      [%log.global.debug
        "Marking collector node as derived-rooted" (Log.node node : Log.node)];
      let was_derived_rooted =
        Rootings.is_derived_rooted_by (Node.rootings node) generation.region.id
      in
      (* Safety: this operation is not protected by a write barrier.
         There are two cases:
         1. [node] belongs to [generation] after level updates.
            In this case, we don't trigger a write barrier since we're currently
            generalizing [generation]
         2. [node] belongs some to ancestor region of [generation].
            In which case, we just trigger the write barrier after.
            But this is still safe. *)
      Node.Rooting.unsafe_update node ~f:(fun rootings ->
        Rootings.add_derived rootings generation.region.id);
      if not was_derived_rooted
      then
        (* If this is a cross-region link *)
        if Generation.mem_after_level_updates generation node
        then (
          [%log.global.debug
            "Tracing rooted collector node's children" (Node.id node : Identifier.t)];
          visit_children node)
        else
          (* There are two cases:

           1. The node *was* a member of the generation. In this case, it's level has
              been lowered in [update_regions] and it's region will be marked as dirty
              in [sweep_generation].

           2. The node isn't a member of the generation. This must be a *old* cross-region
              link. In this case, do nothing since we have unset and reset the transitive
              guard, resulting in a noop.

          Both cases involve us doing nothing! *)
          ()
    and visit_children node = S.iter (Node.structure node) ~f:visit in
    (* Trace all nodes reachable from roots, deriving their rooting from [region]. *)
    List.iter rooted_nodes ~f:visit_children;
    (* Invalidate older cross-region links whose derived rooting changed. *)
    List.iter cross_region_nodes ~f:(fun (node, was_derived_rooted) ->
      let is_derived_rooted =
        Rootings.is_derived_rooted_by (Node.rootings node) generation.region.id
      in
      if Bool.(was_derived_rooted <> is_derived_rooted)
      then (
        [%log.global.debug
          "Cross-region derived rooting changed" (Log.node node : Log.node)];
        Node.write_barrier ~state node))
  ;;

  let sweep_generation ~state ~promote ~finalize (generation : Generation.t) =
    [%log.global.debug
      "Sweeping collector region" (Log.region generation.region : Log.region)];
    (* Notify all potentially finalizable nodes.

       Finalization may start work which can still update the node.
       The client must root that work directly before [finalize] returns. *)
    Region.nodes generation.region
    |> List.iter ~f:(fun node ->
      if
        Generation.mem_after_level_updates generation node
        && Node.is_representative node
        && not (Rootings.is_rooted (Node.rootings node))
      then (
        [%log.global.debug
          "Finalizing unrooted collector node" (Log.node node : Log.node)];
        finalize node));
    (* Now we can sweep *)
    let live_nodes =
      Region.nodes generation.region
      |> List.filter ~f:(fun node ->
        if Generation.mem_after_level_updates generation node
        then (
          Node.unsafe_update_desc node (fun desc ->
            let status =
              if not (Rootings.is_rooted desc.rootings)
              then Node_status.Dead
              else
                (* The node has had its updates propagated (regions and rootings).
                   So we can clear its dirty bit. *)
                Live { dirty = false }
            in
            { desc with status });
          [%log.global.debug "Swept collector node" (Log.node node : Log.node)];
          (* Only keep representatives in the region. Non-representatives are just useless
             aliases! *)
          Node.is_representative node && Node.is_live node)
        else (
          (* Register [node] in its promoted region. *)
          Region.register_term ~state (Node.region node) node;
          (* Promote the node *)
          [%log.global.debug "Relocating collector node" (Log.node node : Log.node)];
          promote node;
          (* Filter the node from the current region. *)
          false))
    in
    (Region.pool generation.region).terms <- live_nodes;
    (* Update whether the region is alive / dead *)
    if List.is_empty live_nodes
    then (
      State.mark_dead_region state generation.region;
      [%log.global.debug
        "Collector region is dead" (Log.region generation.region : Log.region)])
    else
      [%log.global.debug
        "Collector region remains alive" (Log.region generation.region : Log.region)]
  ;;

  type 'a collect =
    state:State.t
    -> before_mark:(unit -> unit)
    -> before_sweep:(Region.t -> unit)
    -> promote:(Node.t -> unit)
    -> finalize:(Node.t -> unit)
    -> after_sweep:(unit -> unit)
    -> 'a

  let unsafe_collect_region ~state ~before_sweep ~promote ~finalize region =
    (* Safety: dirty descendants of [region] must already have been processed.
       [generalize_region] and [generalize_all_regions] establish this by
       draining the dirty tree bottom-up. *)
    [%log.global.debug "Beginning collector pass" (Log.region region : Log.region)];
    let generation = Generation.create region in
    update_regions ~state generation;
    let rooted_nodes, cross_region_nodes =
      clear_current_derived_and_find_roots generation
    in
    mark_rooted ~state ~rooted_nodes ~cross_region_nodes generation;
    before_sweep region;
    sweep_generation ~state ~promote ~finalize generation;
    [%log.global.debug "Finished collector pass" (Log.region region : Log.region)]
  ;;

  let collect_region
        ~(state : State.t)
        ~before_mark
        ~before_sweep
        ~promote
        ~finalize
        ~after_sweep
        region
    =
    Tree.With_dirty.drain_dirty
      state.region_tree
      region
      ~before:before_mark
      ~f:(unsafe_collect_region ~state ~before_sweep ~promote ~finalize)
      ~after:after_sweep
  ;;

  let collect_all_regions
        ~(state : State.t)
        ~before_mark
        ~before_sweep
        ~promote
        ~finalize
        ~after_sweep
        ()
    =
    Tree.With_dirty.drain_dirty_roots
      state.region_tree
      ~before:before_mark
      ~f:(unsafe_collect_region ~state ~before_sweep ~promote ~finalize)
      ~after:after_sweep
  ;;

  let unsafe_collect_region
        ~state
        ~before_mark
        ~before_sweep
        ~promote
        ~finalize
        ~after_sweep
        region
    =
    before_mark ();
    unsafe_collect_region ~state ~before_sweep ~promote ~finalize region;
    after_sweep ()
  ;;
end
