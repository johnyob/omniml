open! Import
module Self = Types.Type
module Ident = Self.Ident
module Var = Self.Var

type t = Self.t =
  | Var of Var.t
  | Arrow of t * t
  | Tuple of t list
  | Constr of t list * Ident.t
  | Shape of t list * Types.Principal_shape.t
  | Poly of Self.Scheme.t
[@@deriving sexp, equal, compare, hash]

let var v = Var v
let ( @-> ) t1 t2 = Arrow (t1, t2)
let constr ts constr = Constr (ts, constr)

let tuple ts =
  if List.length ts < 2
  then raise (Invalid_argument "Type.tuple: expect components to be of length >= 2");
  Tuple ts
;;

let shape ts sh = Shape (ts, sh)
let poly scm = Poly scm

module Scheme = struct
  type t = Self.Scheme.t =
    { quantifiers : Var.t list
    ; body : Self.t
    }
  [@@deriving sexp, equal, compare, hash]

  let create ?(quantifiers = []) body = { quantifiers; body }
end

module Matchee = struct
  type t =
    | Arrow of Var.t * Var.t
    | Tuple of Var.t list
    | Constr of Var.t list * Ident.t
    | Poly of Scheme.t
  [@@deriving sexp]
end
