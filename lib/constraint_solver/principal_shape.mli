open! Import

(** Shapes are defined by the constraint language *)
include module type of Types.Principal_shape

module Poly : sig
  include module type of Poly
  include Invariant.S with type t := t

  val create : ?quantifiers:Type.Var.t list -> Type.Scheme.t -> t
end

include Comparable.S with type t := t

val ( @-> ) : t
val constr : arity:int -> Type.Ident.t -> t
val tuple : int -> t
val poly : Type.Scheme.t -> t

(** [arity t] is the arity of the shape [t]. *)
val arity : t -> int

(** [quantifiers t] returns the quantified shape variables in [t]. *)
val quantifiers : t -> Type.Var.t list

(** [poly_shape_decomposition_of_scheme scm] returns the canonical principal decomposition [(ts, poly_sh)] s.t [scm = apply_shape ts poly_sh]. *)
val poly_shape_decomposition_of_scheme : Type.Scheme.t -> Type.t list * Poly.t

module Var : sig
  type shape := t

  module Handler : sig
    type t =
      { run : shape -> unit
        (** [run shape] runs the handler, where [shape] is the filled shape.  *)
      ; default : unit -> unit (** [default ()] is used to fill the variable (or fail). *)
      ; error : unit -> Omniml_error.t
        (** [error ()] is used to generate an error if the shape 
            variable cannot be defaulted. *)
      }
    [@@deriving sexp_of]
  end

  (** A write-once cell containing a principal shape. *)
  type t [@@deriving sexp_of]

  type shape_var := t

  module Region : sig
    (** Shape variables are grouped into regions. Regions are used to 
        decide when it is valid to {e default} remaining empty variables. *)
    type t [@@deriving sexp_of]

    val create : unit -> t

    type region := t

    module Tree : sig
      type node = region Tree.With_dirty.Node.t [@@deriving sexp_of]
      type t = region Tree.With_dirty.t [@@deriving sexp_of]
    end

    val is_empty : Tree.node -> bool
  end

  val id : t -> Identifier.t

  (** [add_handler t h] adds a handler to the shape var that is scheduled 
      once the variable is filled. If the shape is already filled, then 
      the handler is scheduled immediately. *)
  val add_handler : t -> Handler.t -> unit

  exception Empty

  (** [peek_exn t] returns the current content of the cell.
       
      @raise Empty when [t] is empty. *)
  val peek_exn : t -> shape

  (** [errors t] returns the list of errors associated with cancelling 
      the handlers on [t]. *)
  val errors : t -> Omniml_error.t list

  val is_empty : t -> bool

  exception Unify of t * t

  module State : sig
    type t [@@deriving sexp_of]

    val create : id_source:Identifier.source -> t
    val root_region : t -> Region.Tree.node
    val is_quiet : t -> bool
    val num_partially_generalized_regions : t -> int
    val remaining : t -> shape_var list
  end

  exception Not_empty

  (** [fill_exn t s] fills [t] with shape [s] if [t] was empty. 

      @raise Not_empty when [t] is filled with [s'] and [s <> s']. *)
  val fill_exn : state:State.t -> t -> shape -> unit

  (** [create ~id_source ~state] returns an empty shape var. *)
  val create
    :  id_source:Identifier.source
    -> state:State.t
    -> region:Region.Tree.node
    -> t

  (** [collect_rehome_and_default rn] performs three steps on the region [t] rooted at [rn]: 

      1. {b Collection.} Remove any filled variables. These variables are not interesting 
         for defaulting.

      1. {b Rehoming.} Rehome any variables in [t] that are no-longer owned by [t] (due to 
         changes on the variables' regions). Such variables are moved to their native 
         region. 

      2. {b Defaulting.} After rehoming, attempt to default every {e unguarded} shape variable. 
    *)
  val collect_rehome_and_default : state:State.t -> Region.Tree.node -> unit

  val collect_rehome_and_default_roots : state:State.t -> unit
  val collect_rehome_and_error : state:State.t -> Region.Tree.node -> Omniml_error.t list
  val collect_rehome_and_error_roots : state:State.t -> Omniml_error.t list
  val unify : state:State.t -> t -> t -> unit

  (** [unsafe_set_region_if_ancestor t rn] sets [t]'s region to [rn] 
      if it is an ancestor of [t]'s region. 

      Safety: The comparison of regions is determined by levels. *)
  val unsafe_set_region_if_ancestor : state:State.t -> t -> Region.Tree.node -> unit

  (** Guards 

      Shape variables cannot be guarded directly, only transitively. *)
  module Guard = Guard_set.Transitive_guard

  (** [add_guard t g] adds [g] to [t]'s guards. *)
  val add_guard : state:State.t -> t -> Guard.t -> unit

  (** [remove_guard t g] removes the guard [g] from [t]. *)
  val remove_guard : state:State.t -> t -> Guard.t -> unit

  (** [clear_guard t g] clears the guard [g] from [t]. *)
  val clear_guard : state:State.t -> t -> Guard.t -> unit
end
