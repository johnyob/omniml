module type S = sig
  type 'a structure

  module Term : sig
    (** [t] represents a term *)
    type t [@@deriving sexp_of]

    (** [make structure] creates a new unification type w/ structure [structure]. *)
    val create : t structure -> t

    (** [structure t] returns the structure of [t]. *)
    val structure : t -> t structure

    (** [set_structure t structure] sets the structure of [t] to [structure]. *)
    val set_structure : t -> t structure -> unit

    (** [is_representative t] returns whether [t] is the representative of it's equivalence class. *)
    val is_representative : t -> bool

    (** [same_class t1 t2] returns whether [t1] and [t2] are in the same equivalence class. *)
    val same_class : t -> t -> bool
  end

  module Make_unify (M : Structure.Merge with type 'a t := 'a structure) : sig
    (** [unify ~ctx t1 t2] equates the types [t1] and [t2].

        [Unify (t1, t2)] is raised if the two node cannot be unified. *)

    exception Unify of Term.t * Term.t

    val unify : ctx:Term.t M.ctx -> Term.t -> Term.t -> unit

    (** [try_unify_or_rollback ~ctx t1 t2] unifies [t1] and [t2]. If this 
        raises [Unify], then any changes will be undone.  *)
    val try_unify_or_rollback : ctx:Term.t M.ctx -> Term.t -> Term.t -> unit
  end
end

module Make (S : Structure.Basic) : S with type 'a structure := 'a S.t
