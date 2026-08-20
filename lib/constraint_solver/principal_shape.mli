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

  module State : sig
    type t [@@deriving sexp_of]

    val create : id_source:Identifier.source -> t
    val is_quiescent : t -> bool
    val num_alive_regions : t -> int
    val shape_vars : t -> shape_var list
  end

  module Region : sig
    (** Shape variables are grouped into regions. Regions are used to 
        decide when it is valid to {e default} remaining empty variables. *)
    type t [@@deriving sexp_of]

    val root : state:State.t -> t
    val create : state:State.t -> parent:t -> t
  end

  val id : t -> Identifier.t

  (** [is_empty t] returns true when the cell is empty. *)
  val is_empty : t -> bool

  (** [defaulted t] returns whether [t] was defaulted. *)
  val defaulted : t -> bool

  exception Empty

  (** [shape_exn t] returns the current contents of the cell.

      @raises Empty if [t] is empty. *)
  val shape_exn : t -> shape

  (** [add_handler t h] adds a handler to the shape var that is scheduled
      once the variable is filled.

      If the shape is already filled, then the handler is scheduled immediately. *)
  val add_handler : t -> scheduler:Scheduler.t -> Handler.t -> unit

  exception Not_empty

  (** [fill_exn t s] fills [t] with shape [s] if [t] was empty.

      @raise Not_empty when [t] is filled with [s'] and [s <> s']. *)
  val fill_exn : t -> scheduler:Scheduler.t -> shape -> unit

  (** [create ~id_source ~state ?structure ()] returns a fresh shape variable,
      optionally initialized with [structure]. *)
  val create
    :  state:State.t
    -> region:Region.t
    -> ?defaulted:bool
    -> ?shape:shape
    -> unit
    -> t

  exception Unify of t * t

  val unify : state:State.t -> scheduler:Scheduler.t -> t -> t -> unit

  type 'a generalize = state:State.t -> on_generalize:(t -> unit) -> 'a

  (** [is_generic t] is true when [t] can no longer be updated. *)
  val is_generic : t -> bool

  val default_on_generalize : state:State.t -> scheduler:Scheduler.t -> t -> unit
  val cancel_on_generalize : errors:Omniml_error.t list ref -> t -> unit
  val generalize : (Region.t -> unit) generalize
  val generalize_all : (unit -> unit) generalize

  (** [unsafe_lower t rn] sets [t]'s region to [rn] if it is an ancestor of [t]'s region.

      Safety: The comparison of regions is determined by levels. *)
  val unsafe_lower : state:State.t -> t -> into:Region.t -> unit

  (** [add_guard t g] adds [g] to [t]'s guards. *)
  val add_guard : state:State.t -> t -> Identifier.t -> unit

  (** [remove_guard t g] removes the guard [g] from [t]. *)
  val remove_guard : state:State.t -> t -> Identifier.t -> unit

  (** [clear_guard t g] clears the guard [g] from [t]. *)
  val clear_guard : state:State.t -> t -> Identifier.t -> unit
end
