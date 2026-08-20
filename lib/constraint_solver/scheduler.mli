type job = unit -> unit

(** [t] is a scheduler, a queue of [job]s that are to be run. *)
type t [@@deriving sexp_of]

(** [create ()] returns an empty scheduler. *)
val create : unit -> t

(** [is_empty t] returns true if the job queue is empty. *)
val is_empty : t -> bool

(** [is_maintenance_empty t] returns true when [t] has no deferred internal
    solver updates. Pending handler callbacks are ignored. *)
val is_maintenance_empty : t -> bool

(** [enqueue t job] enqueues the [job] in the scheduler [t]. *)
val enqueue : t -> job -> unit

(** [enqueue_all t jobs] enqueues the [job]s in the scheduler [t]. *)
val enqueue_all : t -> job list -> unit

(** [enqueue_handler t job] enqueues a user constraint handler. Handler jobs
    never run re-entrantly. *)
val enqueue_handler : t -> job -> unit

(** [enqueue_fallback t job] enqueues a non-reentrant fallback handler.
    Fallbacks run only when maintenance work and ordinary handlers are both
    empty. Work produced by an ordinary handler therefore runs before any
    pending fallback. *)
val enqueue_fallback : t -> job -> unit

(** [run t] runs all jobs in [t]. A nested call from a handler may flush
    maintenance jobs, but never enters another handler. *)
val run : t -> unit

(** [run_maintenance t] runs only deferred internal solver updates. It never
    enters a user constraint handler, so it is safe at synchronous collector
    phase boundaries. *)
val run_maintenance : t -> unit

(** [clear t] clears {e all} jobs in [t]. *)
val clear : t -> unit
