type job = unit -> unit

(** [t] is a scheduler, a queue of [job]s that are to be run. *)
type t [@@deriving sexp_of]

(** [t ()] returns the *global scheduler*. *)
val t : unit -> t

(** [is_empty t] returns true if the job queue is empty. *)
val is_empty : t -> bool

(** [enqueue t job] enqueues the [job] in the scheduler [t]. *)
val enqueue : t -> job -> unit

(** [enqueue_all t jobs] enqueues the [job]s in the scheduler [t]. *)
val enqueue_all : t -> job list -> unit

(** [run t] runs {e all} jobs in [t]. *)
val run : t -> unit

(** [clear t] clears {e all} jobs in [t]. *)
val clear : t -> unit
