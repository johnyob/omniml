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

module Principal_shape : sig
  (** Principal shapes describe the "structure" of a type.

      A shape [ν'a .. 'c. ty] denotes some structure [ty] 
      where the variables ['a, .., 'c] are 'holes' within 
      [ty]. 

      A shape is principal if it is minimal with respect 
      to the instantiation of its quantifiers and is 
      non-trivial (i.e. not [ν'a. 'a]).

      This is used by {!Constraint.match_} for defaulting 
      when a type variable's structure cannot be determined 
      from context. *)

  (** [t] represents a principal shape. *)
  type t

  (** [( @-> )] is the principal shape of function types. *)
  val ( @-> ) : t

  (** [constr ~arity id] is the principal shape of the type constructor [id]
      with the given [arity]. *)
  val constr : arity:int -> Type.Ident.t -> t

  (** [tuple n] is the principal shape of a tuple with [n] components. *)
  val tuple : int -> t

  (** [poly scm] is the principal shape of a polymorphic type with scheme [scm]. *)
  val poly : Type.Scheme.t -> t
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

  (** [let_binding] is a constrainted type scheme with a number of binders.

      [forall overline(a). C => (x1 : ty1, ..., xn : tyn)]. *)
  and let_binding

  (** [binding] is a type binding [x : ty], stored within a {!let_binding}. *)
  and binding

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

  (** [x @: ty] constructs the binding [x : ty]. *)
  val ( @: ) : Var.t -> Type.t -> binding

  (** Building constrained type schemes using infix operators. 
      The constrained scheme 
      {[
        forall vs. C => (x1 : ty1, ..., xn: tyn)
      ]}
      is represented (compositionally) as 
      [vs @. c @=> (x1 : ty1, ..., xn : tyn)]. 

      The following combinators and types exist purely to support 
      this syntax. *)

  type unquantified_let_binding := t * binding list

  type quantified_let_binding :=
    (flexibility * Type.Var.t) list * unquantified_let_binding

  val ( @=> ) : t -> binding list -> unquantified_let_binding

  val ( @. )
    :  (flexibility * Type.Var.t) list
    -> unquantified_let_binding
    -> quantified_let_binding

  (** [mono_binding bindings] builds a monomorphic let binding.
      This is equivalent to [poly_binding ([] @. tt @=> bindings)]. *)
  val mono_binding : binding list -> let_binding

  (** [poly_binding (vs @. c @=> bindings)] builds the constrained let binding
      [forall vs. c => bindings]. *)
  val poly_binding : quantified_let_binding -> let_binding

  (** [let_ binding ~in_:c] binds the variables in [binding] in [c]. *)
  val let_ : let_binding -> in_:t -> t

  (** [inst x ty] requires the type [ty] to be an instance of the scheme 
      bound to [x]. The variable [x] must be bound earlier by {!let_}. *)
  val inst : Var.t -> Type.t -> t

  module Match_error : sig
    type t =
      | Cannot_default
      (** [Cannot_default] is raised when [match v ...] could not be 
          defaulted since we could not determine that [v] is *never* 
          determined. *)
      | Matchee_is_rigid
      (** [Matchee_is_rigid] is raised when [match v ...] fails because [v] 
          is unified with a rigid type variable. *)
      | Inconsistent_default of
          { actual : Principal_shape.t
          ; expected : Principal_shape.t
          }
      (** [Inconsistent_default { actual; expected }] occurs when 
          [match v ... else_:(fun () -> expected)] differs from the 
          current shape of [v]. This occurs when one default has 
          previously succeeded (with shape [actual]). *)
    [@@deriving sexp]
  end

  (** [match v ...] matches on the "shape" of [v] and has the following 
      interpretation:

      - [with_] is called when the context in which the match appears 
        refines [v] to a concrete type, and is given a {!Type.Matchee.t} 
        to describe the shape of said type. 

      - [else_] is used when [v] is *never* determined by the surrounding context. 
        If [else_ ()] is [sh], then [v] is unified s.t [shape(v) = sh],
        triggering the associated case of [sh]. 
      
      - [error] is used when we cannot determine that [v] is *never* determined. 
        
     The [closure] parameter specifies which variables are permitted to 
     appear in the generated constraints. *)
  val match_
    :  Type.Var.t
    -> closure:[< `Type of Type.Var.t | `Scheme of Var.t ] list
    -> with_:(Type.Matchee.t -> t)
    -> else_:(unit -> Principal_shape.t)
    -> error:(Match_error.t -> Omniml_error.t)
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

(** [solve ?range c] solves the constraint [c]. 

     On success returns [Ok ()]. On failure returns a structured 
     {!Error.t}. If [range] is provided, it is used as a fallback 
     location for errors that are not already range-annotated 
     via {!Constraint.with_range}. *)
val solve
  :  ?range:Range.t
  -> ?defaulting:Omniml_options.Defaulting.t
  -> Constraint.t
  -> (unit, Error.t) result
