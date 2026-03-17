open! Import

(** Guard sets. 

    A value of type [t] records whether an object is *guarded*. 

    We distinguish two kinds of guards:
    - **Direct guards** represent guards explicitly registered on the object. 

    - **Transitive guards** represent guards that reach the object indirectly 
      (via some predecessory dependency). Each transitive guard is associated 
      with a unique identifier.*)
module Transitive_guard = Identifier

type t [@@deriving sexp_of]

(** The empty guard set. *)
val empty : t

(** [is_empty t] is [true] iff [t] contains no direct nor transitive guards. *)
val is_empty : t -> bool

(** [add_guard t] adds a {e direct} guard to [t]. *)
val add_guard : t -> t

(** [remove_guard t] removes a {e direct} guard from [t]. 
     If no direct guards are present, then [t] is returned. *)
val remove_guard : t -> t

(** [is_transitively_guarded t ~by] holds if [by] is among the transitive 
    guards of [t]. *)
val is_transitively_guarded : t -> by:Transitive_guard.t -> bool

(** [add_transitive_guard t tg] increments the count for the transitive 
    guard [tg] on[t] *)
val add_transitive_guard : t -> Transitive_guard.t -> t

(** [remove_transitive_guard t tg] decrements the count for the transitive 
    guard [tg] from [t]. 

    @raises Invalid_argument if [tg] is not present in the transitive guards 
                             of [t]. *)
val remove_transitive_guard : t -> Transitive_guard.t -> t

(** [clear_transitive_guard t tg] removes all transitive guard [tg] from [t]. 
    (a no-op if absent) *)
val clear_transitive_guard : t -> Transitive_guard.t -> t

(** [union t1 t2] computes the union of the guard sets [t1] and [t2]. *)
val union : t -> t -> t
