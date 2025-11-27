open! Import
open Constraint

let empty_env_wrapper f = f (Env.empty ())

let stdlib_wrapper ?(with_stdlib = true) f =
  if with_stdlib then Predef.Env.wrap f else empty_env_wrapper f
;;

let infer_exp ?with_stdlib exp =
  stdlib_wrapper ?with_stdlib
  @@ fun env ->
  let exp_type = Type.Var.create ~id_source:(Env.id_source env) ~name:"exp_type0" () in
  let c = Infer.Expression.infer_exp ~env exp exp_type in
  exists exp_type c
;;

let infer_str ?with_stdlib str =
  stdlib_wrapper ?with_stdlib @@ fun env -> Infer.Structure.infer_str ~env str
;;

let check ?defaulting cst =
  match Omniml_constraint_solver.(solve ?defaulting cst) with
  | Ok () -> ()
  | Error { range; it } ->
    let get_range range =
      Option.value_or_thunk range ~default:(fun () ->
        Omniml_error.(
          raise
          @@ bug_s
               ~here:[%here]
               [%message
                 "Expect range to be given"
                   (it : Omniml_constraint_solver.Error.desc)
                   (cst : Constraint.t)]))
    in
    (match it with
     | Unsatisfiable err -> Omniml_error.raise err
     | Cannot_discharge_match_constraints errs -> Omniml_error.(raise @@ all errs)
     | Unbound_type_var type_var ->
       Omniml_error.(
         raise
         @@ bug_s
              ~here:[%here]
              [%message
                "Unbound constraint type variable"
                  (type_var : Constraint.Type.Var.t)
                  (range : Range.t option)
                  (cst : Constraint.t)])
     | Unbound_var var ->
       Omniml_error.(
         raise
         @@ bug_s
              ~here:[%here]
              [%message
                "Unbound constraint variable"
                  (var : Constraint.Var.t)
                  (range : Range.t option)
                  (cst : Constraint.t)])
     | Cannot_unify (type1, type2) ->
       Omniml_error.(
         raise
         @@ mismatched_type
              ~range:(get_range range)
              ~pp_type:Omniml_constraint_solver.Decoded_type.pp
              type1
              type2)
     | Rigid_variable_escape ->
       Omniml_error.(raise @@ rigid_variable_escape ~range:(get_range range)))
;;
