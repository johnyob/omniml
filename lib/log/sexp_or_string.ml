open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  ]
[@@deriving sexp]

let to_string = function
  | `Sexp sexp -> Sexp.to_string sexp
  | `String str -> str
;;
