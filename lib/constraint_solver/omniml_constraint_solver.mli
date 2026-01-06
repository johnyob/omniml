open! Import

module For_testing : sig
  (** Helpers intended for tests. 

      This re-exports some internal modules and some quickcheck generators. *)

  module Type = Type
  module Principal_shape = Principal_shape

  module Quickcheckable : sig
    module Type : sig
      module Scheme : Quickcheckable.S with type t := Type.Scheme.t
      include Quickcheckable.S with type t := Type.t
    end

    module Principal_shape : Quickcheckable.S with type t := Principal_shape.t
  end
end

module Type : sig
  (** Concrete type representation used inside constraints. 

      Types contain *type variables* (see {!Var}). *)

  module Ident : Var.S
  module Var : Var.S

  (** [t] represents a type [tau]. *)
  type t [@@deriving sexp, equal, compare, hash]

  type type_ := t

  module Scheme : sig
    (** [t] represents a polymorphic type [forall 'a1, ..., 'an. tau] *)
    type t =
      { quantifiers : Var.t list
      ; body : type_
      }
    [@@deriving sexp, equal, compare, hash]

    (** [create ?quantifiers ty] builds the scheme [forall quantifiers. ty].

        If [quantifiers] is omitted, the scheme is treated as monomorphic. *)
    val create : ?quantifiers:Var.t list -> type_ -> t
  end

  (** [var v] is the type variable [v]. *)
  val var : Var.t -> t

  (** [a @-> b] is the function type [a -> b]. *)
  val ( @-> ) : t -> t -> t

  (** [constr args id] is the constructor application [args id]
      (e.g. [t list] or [(a, b) result]). *)
  val constr : t list -> Ident.t -> t

  (** [tuple ts] is the tuple type [(t1 * ... * tn)]. 

      @raises Invalid_argument if [length ts] is less than 2. *)
  val tuple : t list -> t

  (** [poly s] embeds a type scheme as a (mono)type. *)
  val poly : Scheme.t -> t

  module Matchee : sig
    (** A "pattern" used by {!Constraint.match_}.

        A matchee describes the outer form of a type by listing the type
        variables that stand for its immediate components. This corresponds 
        to the principal shape of the type being matched where the shape 
        quantifiers are implicitly bound. *)
    type t =
      | Arrow of Var.t * Var.t
      (** [Arrow (arg, ret)] is a function type with argument [arg]
          and result [ret] variables. *)
      | Tuple of Var.t list
      (** [Tuple vs] is a tuple type with component variables [vs]. *)
      | Constr of Var.t list * Ident.t
      (** [Constr (vs, id)] is the nominal type with argument variables [vs]
          and type constructor id. *)
      | Poly of Scheme.t (** [Poly scm] is a polymorphic scheme node. *)
    [@@deriving sexp]
  end
end

module Constraint : sig
  (** Variables standing for term/program variables in constraints. *)
  module Var : Var.S

  module Closure : sig
    (** A set of variables considered "in scope" for a match.

        This is used to control what may be referenced by branches produced
        by {!match_}. *)
    type t =
      { type_vars : Type.Var.t list
      ; vars : Var.t list
      }
    [@@deriving sexp]
  end

  (** [t] is a constraint. *)
  type t

  (** [scheme] is a constrainted type scheme [forall overline(a). C => tau]. *)
  and scheme

  (** [let_binding] is a constrainted type scheme binding [x = σ], 
      consumed by {!let_}. *)
  and let_binding

  (** Whether a quantified type variable must be generalizable. *)
  and flexibility =
    | Flexible
    (** May be monomorphic or generalised 
        (depends on the constraint). *)
    | Rigid (** Must not be monomorphic
        escaping rigid variables is an error. *)
  [@@deriving sexp]

  (** The trivially satisfiable constraint. *)
  val tt : t

  (** An unsatisfiable constraint. *)
  val ff : Omniml_error.t -> t

  (** [c1 &~ c2] is the logical conjunction of [c1] and [c2]. *)
  val ( &~ ) : t -> t -> t

  (** [all cs] is the logical conjunction of all constraints in [cs]. 
      
      If [cs] is [[]], then the constraint is trivially true. *)
  val all : t list -> t

  (** [ty1 =~ ty2] asserts the equality of [ty1] and [ty2] as a constraint. *)
  val ( =~ ) : Type.t -> Type.t -> t

  (** [exists v c] existentially binds the type variable [v] in the 
      constraint [c]. *)
  val exists : Type.Var.t -> t -> t

  (** [exists vs c] existentially binds the type variables [vs] in 
      the constraint [c]. *)
  val exists_many : Type.Var.t list -> t -> t

  (** [forall vs c] universally binds the type variables [vs] in 
      the constraint [c]. *)
  val forall : Type.Var.t list -> t -> t

  (** [x #= σ] constructs the let binding [x = σ]. *)
  val ( #= ) : Var.t -> scheme -> let_binding

  (** Building constrained type schemes using infix operators. 
      The constrained scheme 
      {[
        forall vs. C => ty
      ]}
      is represented (compositionally) as [vs @. c @=> ty]. 

      The following combinators and types exist purely to support 
      this syntax. *)

  type unquantified_scheme := t * Type.t
  type quantified_scheme := (flexibility * Type.Var.t) list * unquantified_scheme

  val ( @=> ) : t -> Type.t -> unquantified_scheme
  val ( @. ) : (flexibility * Type.Var.t) list -> unquantified_scheme -> quantified_scheme

  (** [mono_scheme ty] builds a monomorphic scheme. 
      This is equivalent to [poly_scheme ([] @. tt @=> ty)]. *)
  val mono_scheme : Type.t -> scheme

  (** [poly_scheme (vs @. c @=> ty)] builds the constrained scheme 
      [forall vs. c => ty]. *)
  val poly_scheme : quantified_scheme -> scheme

  (** [let_ x #= s ~in_:c] binds [x] to [s] in [c]. *)
  val let_ : let_binding -> in_:t -> t

  (** [inst x ty] requires the type [ty] to be an instance of the scheme 
      bound to [x]. The variable [x] must be bound earlier by {!let_}. *)
  val inst : Var.t -> Type.t -> t

  (** [match v ...] matches on the "shape" of [v] and has the following 
      interpretation:

      - [with_] is called when the context in which the match appears 
        refines [v] to a concrete type, and is given a {!Type.Matchee.t} 
        to describe the shape of said type. 

      - [else_] is used when [v] is undetermined by the surrounding context. 
        The constraint returned by [else_ ()] *must* unify [v] is a concrete 
        type. Following this, the [with_] handler is invoked. 

      - [error] provides an explanation if the solver doesn't discharge 
        [with_] or [else_]. 

     The [closure] parameter specifies which variables are permitted to 
     appear in the generated constraints. *)
  val match_
    :  Type.Var.t
    -> closure:[< `Type of Type.Var.t | `Scheme of Var.t ] list
    -> with_:(Type.Matchee.t -> t)
    -> else_:(unit -> t)
    -> error:(unit -> Omniml_error.t)
    -> t

  (** [with_range c ~range] is equivalent to [c], but attaches the source 
      range [range]. This information is used for error reporting. See 
      {!Error}. *)
  val with_range : t -> range:Range.t -> t
end

module Decoded_type : sig
  (** A type produced by the constraint solver. *)
  type t [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

module Error : sig
  (** Errors produces by {!solve}. *)
  type t =
    { it : desc
    ; range : Range.t option
    }

  and desc =
    | Unsatisfiable of Omniml_error.t (** The constraint is unsatisfiable. *)
    | Unbound_type_var of Type.Var.t
    (** [Unbound_type_var v] denotes that type variable [v] was 
        referenced out of scope. *)
    | Unbound_var of Constraint.Var.t
    (** [Unbound_var x] denotes that the program variable [x] was 
        referenced out of scope. *)
    | Rigid_variable_escape
    (** A rigid type variable escaped it scopes, meaning that the variable 
        could not be universally quantified/generalized. *)
    | Cannot_unify of Decoded_type.t * Decoded_type.t
    (** [Cannot_unify (ty1, ty2)] indicates that unification failed for 
        the given types [ty1] and [ty2]. *)
    | Cannot_discharge_match_constraints of Omniml_error.t list
    (** Some match constraints could not be discharged. *)
  [@@deriving sexp]
end

(** [solve ?range ?defaulting c] solves the constraint [c]. 

     On success returns [Ok ()]. On failure returns a structured 
     {!Error.t}. If [range] is provided, it is used as a fallback 
     location for errors that are not already range-annotated 
     via {!Constraint.with_range}. *)
val solve
  :  ?range:Range.t
  -> ?defaulting:Omniml_options.Defaulting.t
  -> Constraint.t
  -> (unit, Error.t) result
