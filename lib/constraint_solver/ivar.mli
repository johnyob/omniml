(** A write-once variable containing a value of type ['a].

    Before an ivar is filled, callbacks may be registered with {!upon}. Filling the ivar
    schedules every pending callback and permanently records its value. Ivars may be merged;
    after merging, operations through either ivar affect the same underlying cell. *)
type 'a t [@@deriving sexp_of]

module Handler : sig
  (** A handle for a callback registered with {!upon}. *)
  type 'a t [@@deriving sexp_of]

  (** [cancel t ~scheduler] removes [t] from its ivar and schedules its cancellation
      callback.

      It returns [`Already_cancelled] if [t] was previously cancelled,
      [`Already_scheduled] if its run callback has already been scheduled, and
      [`Cannot_cancel] if it was registered without a cancellation callback. *)
  val cancel
    :  'a t
    -> scheduler:Scheduler.t
    -> [ `Ok | `Already_cancelled | `Already_scheduled | `Cannot_cancel ]
end

(** [create ()] returns a fresh, empty ivar. *)
val create : unit -> 'a t

(** [upon t ~run ?cancel ~scheduler ()] registers [run] to be scheduled when [t] is
    filled. If [t] is already full, [run] is scheduled immediately.

    Supplying [cancel] makes the returned handler cancellable, both individually with
    {!Handler.cancel} and collectively with {!cancel_all}. *)
val upon
  :  'a t
  -> run:('a -> unit)
  -> ?cancel:(unit -> unit)
  -> scheduler:Scheduler.t
  -> unit
  -> 'a Handler.t

(** [fill t value ~scheduler] fills an empty [t] with [value] and schedules all of its
    pending run callbacks. If [t] is already full, it is unchanged and [`Full existing]
    is returned. *)
val fill : 'a t -> 'a -> scheduler:Scheduler.t -> [ `Ok | `Full of 'a ]

(** [cancel_all t ~scheduler] cancels every cancellable pending handler. Handlers
    registered without [cancel] remain pending. Calling this on a full ivar is a no-op. *)
val cancel_all : 'a t -> scheduler:Scheduler.t -> unit

(** [merge t1 t2 ~scheduler ~f] makes [t1] and [t2] refer to the same ivar.

    Pending handlers from both ivars are retained. If exactly one ivar is full, its value
    fills the merged ivar and the other ivar's handlers are scheduled. If both are full,
    [f] combines their values. *)
val merge : 'a t -> 'a t -> scheduler:Scheduler.t -> f:('a -> 'a -> 'a) -> unit
