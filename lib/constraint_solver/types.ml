(* This file defines all the mutually recursive types in our constraint solver. 
   Luckily, there aren't many of them. *)

open! Import

module Type_ident = Var.Make (struct
    let module_name = "Type.Ident"
  end)

module Type_var = Var.Make (struct
    let module_name = "Type.Var"
  end)

module rec Type : sig
  module Ident = Type_ident
  module Var = Type_var

  (** [t] represents the type [tau]. *)
  type t =
    | Var of Var.t (** Type variable ['a] *)
    | Arrow of t * t (** Function types [tua1 -> tau2] *)
    | Tuple of t list (** Tuple types [tau1 * ... * taun] *)
    | Constr of t list * Ident.t (** Nominal types [(tau1, ..., taun) T] *)
    | Shape of t list * Principal_shape.t (** Applied shape [(tau1, ..., taun) s] *)
    | Poly of Type.Scheme.t (** Polytypes [[sigma]] *)
  [@@deriving sexp, equal, compare, hash]

  module Scheme : sig
    (** [t] represents a polymorphic type [forall 'a1, ..., 'an. tau] *)
    type t =
      { quantifiers : Type.Var.t list
      ; body : Type.t
      }
    [@@deriving sexp, equal, compare, hash]
  end
end = struct
  module Ident = Type_ident
  module Var = Type_var

  (** [t] represents the type [tau]. *)
  type t =
    | Var of Var.t (** Type variable ['a] *)
    | Arrow of t * t (** Function types [tua1 -> tau2] *)
    | Tuple of t list (** Tuple types [tau1 * ... * taun] *)
    | Constr of t list * Ident.t (** Nominal types [(tau1, ..., taun) T] *)
    | Shape of t list * Principal_shape.t (** Applied shape [(tau1, ..., taun) s] *)
    | Poly of Type.Scheme.t (** Polytypes [[sigma]] *)
  [@@deriving sexp, equal, compare, hash]

  module Scheme = struct
    (** [t] represents a polymorphic type [forall 'a1, ..., 'an. tau] *)
    type t =
      { quantifiers : Type.Var.t list
      ; body : Type.t
      }
    [@@deriving sexp, equal, compare, hash]
  end
end

and Principal_shape : sig
  module Poly : sig
    type t =
      { quantifiers : Type.Var.t list
      ; scheme : Type.Scheme.t
      }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Sh_arrow (** Arrow shape ['c1 'c2. 'c1 -> 'c2] *)
    | Sh_tuple of int (** Tuple shape ['c1, ..., 'cn. 'c1 * ... * 'cn] *)
    | Sh_constr of int * Type.Ident.t
    (** Nominal type shape ['c1, ..., 'cn. ('c1, ..., 'cn) T] *)
    | Sh_poly of Poly.t (** Polytype shape ['c1, ..., 'cn. [sigma]] *)
  [@@deriving sexp, equal, compare, hash]
end = struct
  module Poly = struct
    type t =
      { quantifiers : Type.Var.t list
      ; scheme : Type.Scheme.t
      }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Sh_arrow (** Arrow shape ['c1 'c2. 'c1 -> 'c2] *)
    | Sh_tuple of int (** Tuple shape ['c1, ..., 'cn. 'c1 * ... * 'cn] *)
    | Sh_constr of int * Type.Ident.t
    (** Nominal type shape ['c1, ..., 'cn. ('c1, ..., 'cn) T] *)
    | Sh_poly of Poly.t (** Polytype shape ['c1, ..., 'cn. [sigma]] *)
  [@@deriving sexp, equal, compare, hash]
end
