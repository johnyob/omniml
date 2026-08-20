open Core
open Omniml_std
open Omniml_unifier

module type Structure = sig
  include Structure.Basic
  include Structure.Iter with type 'a t := 'a t
  include Structure.Merge with type 'a t := 'a t

  module Region_metadata : sig
    type 'a t [@@deriving sexp_of]
  end
end

module type Make = functor (S : Structure) -> sig
  (** The collector determines when nodes can no longer be affected by future
      work. Such nodes become [Dead] and must never be updated again; nodes which
      remain reachable from roots stay [Live].

      Regions arrange nodes by lexical scope. They form a tree whose root is the
      outermost scope and whose children are increasingly local scopes:

                             R0
                            /  \
                           /    \
                          R1    R2
                         /  \
                        R3  R4

      Each live node is owned by exactly one region. A node owned by [R] may
      refer only to live nodes owned by [R] or by an ancestor of [R]:

          owner(parent) = R       owner(child) is R or an ancestor of R

      This is the central region invariant. Moving a node towards the root is
      safe because it makes the node visible from more scopes. Moving it away
      from the root could hide it from an existing reference and is forbidden.

      Unification may merge nodes owned by different regions. The merged class
      must be visible wherever either input was visible, so it is promoted to
      the nearest common ancestor of the two owners. For example:

                             R0
                            /  \
                           /    \
             owns [left]  R1    R2  owns [right]
                         /  \
                        R3  R4

          nearest_common_ancestor(R1, R2) = R0

                             R0  owns [left = right]
                            /  \
                           /    \
                          R1    R2
                         /  \
                        R3  R4

      A region's {e level} is its depth in the tree. The collector processes
      dirty regions from children to parents, so changes discovered in a child
      can safely make an ancestor dirty for a later pass. *)
  module rec Region : sig
    type t [@@deriving sexp_of]

    module Status : sig
      type t =
        | Alive
        | Dead
      [@@deriving sexp_of]
    end

    module Level : sig
      (** The region's level i.e. its depth in the region tree *)
      type t = private int [@@deriving equal, compare, sexp, hash]

      include Comparable.S with type t := t
    end

    (** [create ~state ~curr_region value] creates a new child region as a descendant
        of [curr_region] in the region tree. The new region will have a level one greater
        than its parent. *)
    val create : state:State.t -> parent:t -> Node.t S.Region_metadata.t -> t

    (** [metadata t] returns the value associated with this region. *)
    val metadata : t -> Node.t S.Region_metadata.t

    (** [parent t] returns the parent of the region. [None] if [t] is the root. *)
    val parent : t -> t option

    (** [status t] returns the status of the region. A region is alive if it contains 
        [Live] nodes. *)
    val status : t -> Status.t

    (** [level t] returns the level of the region. *)
    val level : t -> Level.t

    (** [nodes t] returns the nodes currently registered in this region. A dirty
        node may already be owned by an ancestor and will be relocated when its
        old region is next collected. *)
    val nodes : t -> Node.t list
  end

  (** The state of the collector. *)
  and State : sig
    type t [@@deriving sexp_of]

    (** [create ~id_source ~root] creates a new state with a single root region. *)
    val create : id_source:Identifier.source -> root:Node.t S.Region_metadata.t -> t

    (** [root_region t] returns the root of the region tree. *)
    val root_region : t -> Region.t

    (** [is_quiescent t] is [true] when no region requires collection. *)
    val is_quiescent : t -> bool

    (** [alive_regions t] returns a list of alive regions in the collector *)
    val alive_regions : t -> Region.t list

    (** [num_alive_regions t] returns the number of alive regions in the collector. *)
    val num_alive_regions : t -> int
  end

  (** Nodes with collector information. *)
  and Node : sig
    type t [@@deriving sexp_of]

    (** [create ~state ~curr_region structure] creates a new node in the given region
        with the specified structure. *)
    val create : state:State.t -> curr_region:Region.t -> t S.t -> t

    (** [id t] returns the unique identifier of the node. *)
    val id : t -> Identifier.t

    (** [structure t] returns the structure of the node. *)
    val structure : t -> t S.t

    (** [region t] returns the region this node belongs to. *)
    val region : t -> Region.t

    (** [is_live t] returns whether the node is live. *)
    val is_live : t -> bool

    (** [is_dead t] is [not (is_live t)]. *)
    val is_dead : t -> bool

    (** [is_representative t] returns true if this node is the 
        representative of its equivalence class. *)
    val is_representative : t -> bool

    (** [same_class t1 t2] returns true if both nodes belong to the same equivalence class. *)
    val same_class : t -> t -> bool

    exception Unify of t * t

    (** [unify ~state ~curr_region ~ctx t1 t2] unifies two nodes, raising an exception
        if unification fails. *)
    val unify : state:State.t -> curr_region:Region.t -> ctx:t S.ctx -> t -> t -> unit

    (** [try_unify_or_rollback ~state ~curr_region ~ctx t1 t2] attempts to unify two nodes,
        rolling back changes if unification fails. *)
    val try_unify_or_rollback
      :  state:State.t
      -> curr_region:Region.t
      -> ctx:t S.ctx
      -> t
      -> t
      -> unit

    (** [update_structure ~state t ~f] updates the structure of [t],
        performing the write barrier required by the collector. 

        A write barrier notifies the collector of an update that could affect the 
        set of live nodes. *)
    val update_structure : state:State.t -> t -> f:(t S.t -> t S.t) -> unit

    module Unsafe : sig
      (** Replaces the structure without performing a write barrier.

          Safety: Use only while the context owns the node's collection update,
          such as during promotion or finalization. *)
      val set_structure : t -> t S.t -> unit

      (** [promote ~state t ~into] moves [t] into the ancestor region
          [into], applying the necessary write barrier and region bookkeeping.
        
          Safety: [into] must be an ancestor of its current region. *)
      val promote : state:State.t -> t -> into:Region.t -> unit
    end

    (** Write-barrier-protected rooting updates.

        Direct and derived roots are counted, so overlapping paths may
        contribute independently. Derived roots record the result of tracing a
        source region and are partitioned by that region's identifier. *)
    module Rooting : sig
      (** [root ~state t] records a root of [t].

          Safety: each [root] must be balanced by a corresponding [unroot]. *)
      val root : state:State.t -> t -> unit

      (** [unroot ~state t] removes a root of [t]. *)
      val unroot : state:State.t -> t -> unit
    end

    module Mark : sig
      type node := t
      type 'a t [@@deriving sexp_of]

      val create : unit -> 'a t
      val mark : 'a t -> node -> 'a -> bool
    end

    val try_mark : t -> 'a Mark.t -> 'a -> bool
  end

  type 'a collect =
    state:State.t
    -> before_mark:(unit -> unit)
    -> before_sweep:(Region.t -> unit)
    -> promote:(Node.t -> unit)
    -> finalize:(Node.t -> unit)
    -> after_sweep:(unit -> unit)
    -> 'a

  (** Collection is split into a marking phase and a sweeping phase.

      The safe traversal functions below run the phases in this order:

        +--------------------------------------------------------+
        | before_mark ()                                         |
        +--------------------------------------------------------+
        |                                                        |
        | MARK                                                   |
        | 1. trace live nodes reachable from [R]                 |
        | 2. promote nodes whose current owner is too young      |
        | 3. clear and recompute rootings derived from [R]       |
        |                                                        |
        +--------------------------------------------------------+
        | before_sweep R                                         |
        +--------------------------------------------------------+
        |                                                        |
        | SWEEP                                                  |
        | 1. call [promote node] for promoted nodes              |
        | 2. call [finalize node] for eligible nodes             |
        |    note: this may update the node preventing           |
        |          collection                                    |
        | 3. make still-unrooted nodes dead                      |
        | 4. retain rooted nodes as clean, live nodes            |
        |                                                        |
        +--------------------------------------------------------+
        | after_sweep ()                                         |
        +--------------------------------------------------------+

      This process occurs for all dirty region nodes, in a child-to-parent
      order.

      The phase boundaries establish the following invariants:

      - Before marking [R], every dirty descendant of [R] has already been
        collected. Dead nodes are immutable and are never traversed.
      - After marking [R], every visited live node satisfies the region
        invariant, and rootings derived from [R] describe the current graph.
        If a cross-region rooting changed, the owning ancestor is made dirty.
      - [before_sweep R] observes the completed region and rooting updates, but
        runs before any node in [R] is made dead.
      - [finalize node] may perform domain-specific finalization. If it starts
        work which may access [node] later, it must root [node] before returning.
      - After sweeping [R], its unrooted nodes are dead, its retained nodes are
        live and clean, and promoted nodes have been moved to their new owning
        regions. *)

  (** [unsafe_collect_region ~state ~before_mark ~before_sweep ~promote
      ~finalize ~after_sweep region] performs one mark-and-sweep pass for
      [region].

      Safety: every dirty descendant of [region] must have been collected
      first. Prefer {!collect_region} or {!collect_all_regions}, which
      establish this ordering. *)
  val unsafe_collect_region : (Region.t -> unit) collect

  (** [collect_region ~state ~before_mark ~before_sweep ~promote ~finalize
      ~after_sweep region] collects every dirty region in the subtree rooted at
      [region]. Regions are processed child-before-parent and clean subtrees are
      skipped. Newly dirtied regions are processed before the function returns. *)
  val collect_region : (Region.t -> unit) collect

  (** [collect_all_regions ~state ~before_mark ~before_sweep ~promote
      ~finalize ~after_sweep ()] collects every dirty region in the collector.
      Regions are processed child-before-parent and clean subtrees are skipped.
      Newly dirtied regions are processed before the function returns. On return,
      {!State.is_quiescent} is true unless a callback scheduled further updates. *)
  val collect_all_regions : (unit -> unit) collect
end

module type Intf = sig
  module type Structure = Structure

  module Make : Make
end
