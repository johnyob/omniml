open! Import
open Constraint
open Ast_types

(* Pre-defined types have their own [id_source] since the type names cannot be parsed => no conflict possible *)
let id_source = Identifier.create_source ()
let int_ident = Type.Ident.create ~id_source ~name:"Stdlib.int" ()
let bool_ident = Type.Ident.create ~id_source ~name:"Stdlib.bool" ()
let unit_ident = Type.Ident.create ~id_source ~name:"Stdlib.unit" ()
let int = Type.(constr [] int_ident)
let bool = Type.(constr [] bool_ident)
let unit = Type.(constr [] unit_ident)

module Env = struct
  let arg_type ~with_poly_params type_ =
    if with_poly_params then Type.poly (Type.Scheme.create type_) else type_
  ;;

  let bool_bop ~with_poly_params =
    Type.(arg_type ~with_poly_params bool @-> arg_type ~with_poly_params bool @-> bool)
  ;;

  let bool_uop ~with_poly_params = Type.(arg_type ~with_poly_params bool @-> bool)

  let int_bop ~with_poly_params =
    Type.(arg_type ~with_poly_params int @-> arg_type ~with_poly_params int @-> int)
  ;;

  let int_uop ~with_poly_params = Type.(arg_type ~with_poly_params int @-> int)

  let int_comparator ~with_poly_params =
    Type.(arg_type ~with_poly_params int @-> arg_type ~with_poly_params int @-> bool)
  ;;

  let type_def name arity ident =
    { Adt.type_name = Type_name.create name
    ; type_arity = arity
    ; type_ident = ident
    ; type_kind = Type_abstract
    }
  ;;

  let t = [ "int", 0, int_ident; "bool", 0, bool_ident; "unit", 0, unit_ident ]

  let v ~with_poly_params =
    [ "( || )", bool_bop ~with_poly_params
    ; "( && )", bool_bop ~with_poly_params
    ; "not", bool_uop ~with_poly_params
    ; "( = )", int_comparator ~with_poly_params
    ; "( <> )", int_comparator ~with_poly_params
    ; "( < )", int_comparator ~with_poly_params
    ; "( > )", int_comparator ~with_poly_params
    ; "( <= )", int_comparator ~with_poly_params
    ; "( >= )", int_comparator ~with_poly_params
    ; "( + )", int_bop ~with_poly_params
    ; "( - )", int_bop ~with_poly_params
    ; "( * )", int_bop ~with_poly_params
    ; "( / )", int_bop ~with_poly_params
    ; "unary( - )", int_uop ~with_poly_params
    ]
  ;;

  let init () =
    let env = Env.empty () in
    let env =
      List.fold t ~init:env ~f:(fun env (type_str, type_arity, type_ident) ->
        Env.add_type_def env (type_def type_str type_arity type_ident))
    in
    env
  ;;

  let wrap ~with_poly_params k =
    let env = init () in
    let env, bindings =
      List.fold_map (v ~with_poly_params) ~init:env ~f:(fun env (var_str, type_) ->
        Env.rename_var env ~var:(Var_name.create var_str) ~in_:(fun env cvar ->
          env, (cvar, type_)))
    in
    let c = k env in
    List.fold_right bindings ~init:c ~f:(fun (var, type_) in_ ->
      let_ var#=(mono_scheme type_) ~in_)
  ;;
end
