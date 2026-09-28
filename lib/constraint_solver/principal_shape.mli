open! Import

(** Shapes are defined by the constraint language *)
include module type of Types.Principal_shape

module Scheme : sig
  include module type of Scheme
  include Invariant.S with type t := t

  val create : ?quantifiers:Type.Var.t list -> Type.Scheme.t -> t
end

include Comparable.S with type t := t

val ( @-> ) : t
val constr : arity:int -> Type.Ident.t -> t
val tuple : int -> t
val scheme : Type.Scheme.t -> t
val poly : Type.Scheme.t -> t

(** [arity t] is the arity of the shape [t]. *)
val arity : t -> int

(** [quantifiers t] returns the quantified shape variables in [t]. *)
val quantifiers : t -> Type.Var.t list

(** [scheme_shape_decomposition scm] returns the canonical principal decomposition
    [(ts, scheme_shape)] s.t. [scm = apply_shape ts scheme_shape]. *)
val scheme_shape_decomposition : Type.Scheme.t -> Type.t list * Scheme.t

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

  (** A cell containing a principal shape. Cancelled cells retain soft shape
      evidence and can be revived by adding a handler. *)
  type t [@@deriving sexp_of]

  (** [id t] is the identifier of the shape var. *)
  val id : t -> Identifier.t

  (** [is_empty t] returns true when the cell is active and empty. *)
  val is_empty : t -> bool

  (** [is_cancelled t] returns true when the cell is cancelled. *)
  val is_cancelled : t -> bool

  exception Empty

  (** [shape_exn t] returns the current contents of the cell.

      @raises Empty if [t] is empty or cancelled. *)
  val shape_exn : t -> shape

  val shape : t -> shape option

  (** [revive t] returns [t] when it is active. When [t] is cancelled, it
      returns a fresh active variable initialized from [t]'s soft shape evidence.
      The cancelled variable itself is unchanged. *)
  val revive : t -> id_source:Identifier.source -> t

  (** [add_handler t h] adds a handler to the shape var that is scheduled
      once the variable is filled.

      If the shape is already filled, then the handler is scheduled immediately.
      Call [revive] before adding a handler to a potentially cancelled variable. *)
  val add_handler : t -> scheduler:Scheduler.t -> Handler.t -> unit

  exception Not_empty

  (** [fill_exn t s] fills [t] with shape [s] if [t] was empty. A cancelled
      variable records agreeing fills as soft evidence without scheduling handlers;
      a conflicting fill clears that evidence.

      @raise Not_empty when [t] is filled with [s'] and [s <> s']. *)
  val fill_exn : t -> shape -> scheduler:Scheduler.t -> unit

  (** [cancel_exn t] cancels any handlers associated with [t]. Repeated
      cancellation is a no-op.

      @raise Not_empty when [t] is filled with a shape. *)
  val cancel_exn : t -> scheduler:Scheduler.t -> unit

  (** [create ?shape ()] returns a fresh shape variable, optionally initialized with [shape]. *)
  val create : id_source:Identifier.source -> ?shape:shape -> unit -> t

  exception Unify of t * t

  val unify : scheduler:Scheduler.t -> t -> t -> unit
  val try_unify_or_rollback : scheduler:Scheduler.t -> t -> t -> unit
end
