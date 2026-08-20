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

(** [poly_shape_decomposition_of_scheme scm] returns the canonical 
    principal decomposition [(ts, poly_sh)] s.t [scm = apply_shape ts poly_sh]. *)
val poly_shape_decomposition_of_scheme : Type.Scheme.t -> Type.t list * Poly.t

module Var : sig
  type shape := t

  module Handler : sig
    type t =
      { run : shape -> unit
        (** [run shape] runs the handler, where [shape] is the filled shape.  *)
      ; cancel : unit -> unit
        (** [cancel ()] is used to fail and unregister the handler. *)
      }
    [@@deriving sexp_of]
  end

  (** A write-once cell containing a principal shape. *)
  type t [@@deriving sexp_of]

  (** [id t] is the identifier of the shape var. *)
  val id : t -> Identifier.t

  (** [is_empty t] returns true when the cell is empty. *)
  val is_empty : t -> bool

  exception Empty

  (** [shape_exn t] returns the current contents of the cell.

      @raises Empty if [t] is empty. *)
  val shape_exn : t -> shape

  val shape : t -> shape option

  (** [add_handler t h] adds a handler to the shape var that is scheduled
      once the variable is filled.

      If the shape is already filled, then the handler is scheduled immediately. *)
  val add_handler : t -> scheduler:Scheduler.t -> Handler.t -> unit

  exception Not_empty

  (** [fill_exn t s] fills [t] with shape [s] if [t] was empty.

      @raise Not_empty when [t] is filled with [s'] and [s <> s']. *)
  val fill_exn : t -> shape -> scheduler:Scheduler.t -> unit

  (** [cancel_exn t] cancels any handlers associated with [t].

      @raise Not_empty when [t] is filled with a shape. *)
  val cancel_exn : t -> scheduler:Scheduler.t -> unit

  (** [create ?shape ()] returns a fresh shape variable, optionally initialized with [shape]. *)
  val create : id_source:Identifier.source -> ?shape:shape -> unit -> t

  exception Unify of t * t

  val unify : scheduler:Scheduler.t -> t -> t -> unit
  val try_unify_or_rollback : scheduler:Scheduler.t -> t -> t -> unit
end
