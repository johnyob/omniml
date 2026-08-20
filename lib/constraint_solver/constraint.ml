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

type flexibility =
  | Flexible
  | Rigid
[@@deriving sexp_of]

type binding =
  { binding_var : Var.t
  ; binding_type : Type.t
  }
[@@deriving sexp_of]

type _ t =
  | True : unit t
  | Return : 'a -> 'a t
  | False : Omniml_error.t -> 'a t
  | Conj : 'a t * 'b t -> ('a * 'b) t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Eq : Type.t * Type.t -> unit t
  | Exists : Type.Var.t * 'a t -> 'a t
  | Forall : Type.Var.t list * 'a t -> 'a t
  | Let : 'a let_binding * 'b t -> ('a * 'b) t
  | Instance : Var.t * Type.t -> unit t
  | Decode : Type.t -> Decoded_type.t t
  | Match :
      { matchee : Type.Var.t
      ; closure : Closure.t
      ; case : Type.Matchee.t -> unit t
      ; default : unit -> default
      ; error : unit -> Omniml_error.t
      }
      -> unit t
  | With_range : 'a t * Range.t -> 'a t

and default =
  | Shape of Principal_shape.t
  | Constraint of unit t

and 'a let_binding =
  { type_vars : (flexibility * Type.Var.t) list
  ; in_ : 'a t
  ; bindings : binding list
  }

let rec sexp_of_t : type a. a t -> Sexp.t =
  fun t ->
  let atom value = Sexp.Atom value in
  let node name fields = Sexp.List (atom name :: fields) in
  match t with
  | True -> atom "True"
  | Return _ -> atom "Return"
  | False err -> node "False" [ Omniml_error.sexp_of_t err ]
  | Conj (t1, t2) -> node "Conj" [ sexp_of_t t1; sexp_of_t t2 ]
  | Map (t, _) -> sexp_of_t t
  | Eq (type1, type2) -> node "Eq" [ Type.sexp_of_t type1; Type.sexp_of_t type2 ]
  | Exists (type_var, t) -> node "Exists" [ Type.Var.sexp_of_t type_var; sexp_of_t t ]
  | Forall (type_vars, t) ->
    node "Forall" [ [%sexp_of: Type.Var.t list] type_vars; sexp_of_t t ]
  | Let (binding, t) -> node "Let" [ sexp_of_let_binding binding; sexp_of_t t ]
  | Instance (var, type_) -> node "Instance" [ Var.sexp_of_t var; Type.sexp_of_t type_ ]
  | Decode type_ -> node "Decode" [ Type.sexp_of_t type_ ]
  | Match { matchee; closure; case = _; default = _; error = _ } ->
    node
      "Match"
      [ node "matchee" [ Type.Var.sexp_of_t matchee ]
      ; node "closure" [ Closure.sexp_of_t closure ]
      ; node "case" [ atom "<fun>" ]
      ; node "else_" [ atom "<fun>" ]
      ; node "error" [ atom "<fun>" ]
      ]
  | With_range (t, range) -> node "With_range" [ sexp_of_t t; Range.sexp_of_t range ]

and sexp_of_let_binding : type a. a let_binding -> Sexp.t =
  fun { type_vars; in_; bindings } ->
  let field name value = Sexp.List [ Sexp.Atom name; value ] in
  Sexp.List
    [ field "type_vars" ([%sexp_of: (flexibility * Type.Var.t) list] type_vars)
    ; field "in_" (sexp_of_t in_)
    ; field "bindings" ([%sexp_of: binding list] bindings)
    ]
;;

let tt = True
let ff err = False err

include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return value = Return value
    let map = `Custom (fun t ~f -> Map (t, f))
    let apply tf tx = Map (Conj (tf, tx), fun (f, x) -> f x)
  end)

let both t1 t2 = Conj (t1, t2)

module Open_on_rhs_intf = struct
  module type S = sig end
end

module Let_syntax = struct
  let return = return

  include Applicative_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    module Open_on_rhs = struct end
  end
end

let ( &~ ) = both
let ( >> ) t1 t2 = map (both t1 t2) ~f:snd
let ( =~ ) type1 type2 = Eq (type1, type2)
let decode type_ = Decode type_
let exists type_var t = Exists (type_var, t)
let exists_many vars in_ = List.fold_right vars ~init:in_ ~f:exists

let forall type_vars t =
  match type_vars with
  | [] -> t
  | type_vars -> Forall (type_vars, t)
;;

let ( @: ) x type_ = { binding_var = x; binding_type = type_ }
let mono_binding bindings = { type_vars = []; in_ = tt; bindings }
let ( @=> ) t1 t2 = t1, t2
let ( @. ) t1 t2 = t1, t2
let poly_binding (type_vars, (in_, bindings)) = { type_vars; in_; bindings }
let let_ binding ~in_ = Let (binding, in_)
let let_unit binding ~in_ = map (let_ binding ~in_) ~f:(fun _ -> ())
let inst x type_ = Instance (x, type_)

let match_ matchee ~closure ~with_ ~default ~error =
  Match { matchee; closure = Closure.of_list closure; case = with_; default; error }
;;

let with_range t ~range = With_range (t, range)
