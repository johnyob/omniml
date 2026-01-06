open Core
open Omniml_std
open Grace

module Var = Var.Make (struct
    let module_name = "Constraint.Var"
  end)

module Closure = struct
  type t =
    { type_vars : Type.Var.t list
    ; vars : Var.t list
    }
  [@@deriving sexp]

  let of_list type_or_schemes =
    let type_vars, vars =
      List.partition_map type_or_schemes ~f:(function
        | `Type type_var -> First type_var
        | `Scheme var -> Second var)
    in
    { type_vars; vars }
  ;;
end

type t =
  | True
  | False of Omniml_error.t
  | Conj of t * t
  | Eq of Type.t * Type.t
  | Exists of Type.Var.t * t
  | Forall of Type.Var.t list * t
  | Let of Var.t * scheme * t
  | Instance of Var.t * Type.t
  | Match of
      { matchee : Type.Var.t
      ; closure : Closure.t
      ; case : Type.Matchee.t -> t
      ; error : unit -> Omniml_error.t
      ; else_ : unit -> t
      }
  | With_range of t * Range.t

and scheme =
  { type_vars : (flexibility * Type.Var.t) list
  ; in_ : t
  ; type_ : Type.t
  }

and let_binding =
  { let_var : Var.t
  ; let_scheme : scheme
  }

and flexibility =
  | Flexible
  | Rigid
[@@deriving sexp]

let tt = True
let ff err = False err
let ( &~ ) t1 t2 = Conj (t1, t2)

let all ts =
  match ts with
  | [] -> tt
  | [ t ] -> t
  | ts -> List.fold ts ~init:tt ~f:( &~ )
;;

let ( =~ ) type1 type2 = Eq (type1, type2)
let exists type_var t = Exists (type_var, t)
let exists_many vars in_ = List.fold_right vars ~init:in_ ~f:exists
let forall type_vars t = Forall (type_vars, t)
let ( #= ) x scheme = { let_var = x; let_scheme = scheme }
let mono_scheme type_ = { type_vars = []; in_ = tt; type_ }
let ( @=> ) t1 t2 = t1, t2
let ( @. ) t1 t2 = t1, t2
let poly_scheme (type_vars, (in_, type_)) = { type_vars; in_; type_ }
let let_ binding ~in_ = Let (binding.let_var, binding.let_scheme, in_)
let inst x type_ = Instance (x, type_)

let match_ matchee ~closure ~with_ ~else_ ~error =
  Match { matchee; closure = Closure.of_list closure; case = with_; else_; error }
;;

let with_range t ~range = With_range (t, range)
