open! Import

module Level : sig
  (** The depth of a node in a tree *)
  type t = private int [@@deriving equal, compare, sexp, hash]

  include Comparable.S with type t := t
end

module Node : sig
  (** A tree consists of a value and a number of children.
    The level of a node within the tree represents the node's depth. *)
  type 'a t =
    { id : Identifier.t (** Unique identifier of the node *)
    ; level : Level.t
      (** The level of the node in the tree.
      If [parent] is [None], then [level = Level.zero],
      otherwise [level = Level.succ parent.level]. *)
    ; value : 'a (** The region of the node *)
    ; parent : 'a t option (** Parent of the node, if [None] then node is a root node. *)
    }
  [@@deriving sexp_of]

  type 'a opaque_t = 'a t [@@deriving sexp_of]
end

type 'a t = T of 'a Node.t [@@unboxed] [@@deriving sexp_of]

(** [root t] returns the root node of the tree *)
val root : 'a t -> 'a Node.t

(** [create ~id_source v] returns a new tree *)
val create : id_source:Identifier.source -> 'a -> 'a t

(** [create_node ~id_source ~parent v] returns a new tree node with parent [parent] *)
val create_node : id_source:Identifier.source -> parent:'a Node.t -> 'a -> 'a Node.t

(** [nearest_common_ancestor n1 n2] returns the nearest common ancestor of two nodes in a tree *)
val nearest_common_ancestor : 'a Node.t -> 'a Node.t -> 'a Node.t

(** [compare_node_by_level n1 n2] is [Level.compare n1.level n2.level]. *)
val compare_node_by_level : 'a Node.t -> 'a Node.t -> int

module With_dirty : sig
  module Tree_node := Node

  module Node : sig
    (** A dirty-aware node descriptor. 

        This wraps the underlying ['a] with mutable state used to 
        maintain a {e dirty frontier}: a node is either clean or dirty. 

        The node additionally maintains {e overlay} information that provides 
        efficient iteration over dirty descendants (skipping clean nodes in 
        between). *)
    type 'a desc [@@deriving sexp_of]

    (** A tree node whose payload is a dirty-aware descriptor. *)
    type 'a t = 'a desc Tree_node.t [@@deriving sexp_of]

    type 'a opaque_t = 'a desc Tree_node.opaque_t [@@deriving sexp_of]

    (** [value t] returns the underlying wrapped ['a]. *)
    val value : 'a t -> 'a
  end

  (** A tree together with a dirty-frontier overlay. *)
  type 'a t [@@deriving sexp_of]

  (** [root t] returns the root node of [t]. *)
  val root : 'a t -> 'a Node.t

  (** [is_empty t] returns true if there are no dirty nodes *)
  val is_empty : 'a t -> bool

  (** [create ~id_source v] creates a new dirty-aware tree. *)
  val create : id_source:Identifier.source -> 'a -> 'a t

  (** [create_node ~id_source ~parent v] allocates a fresh node with value [v] 
      as a child of [parent]. The node is initially clean. *)
  val create_node : id_source:Identifier.source -> parent:'a Node.t -> 'a -> 'a Node.t

  (** [mark_dirty t n] marks [n] as dirty.

      If [n] is already dirty, this is a no-op. *)
  val mark_dirty : 'a t -> 'a Node.t -> unit

  (** [drain_dirty n ~before ~f ~after] repeatedly processes dirty nodes reachable from 
      [n] until becomes clean. *)
  val drain_dirty
    :  'a t
    -> 'a Node.t
    -> before:(unit -> unit)
    -> f:('a Node.t -> unit)
    -> after:(unit -> unit)
    -> unit

  val drain_dirty_roots
    :  'a t
    -> before:(unit -> unit)
    -> f:('a Node.t -> unit)
    -> after:(unit -> unit)
    -> unit
end
