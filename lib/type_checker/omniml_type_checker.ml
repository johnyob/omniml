open! Import
open Constraint

let empty_env_wrapper f = f (Predef.Env.init ())

let stdlib_wrapper ?(with_stdlib = true) ~with_poly_params f =
  if with_stdlib then Predef.Env.wrap ~with_poly_params f else empty_env_wrapper f
;;

let infer_exp ?with_stdlib ~with_poly_params exp =
  stdlib_wrapper ?with_stdlib ~with_poly_params
  @@ fun env ->
  let exp_type = Type.Var.create ~id_source:(Env.id_source env) ~name:"exp_type0" () in
  let c = Infer.Expression.infer_exp ~with_poly_params ~env exp exp_type in
  exists exp_type c
;;

let infer_str ?with_stdlib ~with_poly_params str =
  stdlib_wrapper ?with_stdlib ~with_poly_params
  @@ fun env -> Infer.Structure.infer_str ~with_poly_params ~env str
;;

let decreasing_instantiation_spec =
  let open Omniml_constraint_solver in
  let open Termination.Budget.Spec in
  (module struct
    module Size = struct
      let rec of_decoded_type (decoded_type : Decoded_type.t) =
        match decoded_type with
        | Var _ | Mu _ -> 0
        | App (decoded_types, _) ->
          1 + List.sum (module Int) decoded_types ~f:of_decoded_type
      ;;
    end

    type t = { sizes : int Constraint.Var.Map.t } [@@deriving sexp_of, compare]

    let initial = { sizes = Constraint.Var.Map.empty }

    let consume var expected_type t =
      let curr_size = Size.of_decoded_type (Lazy.force expected_type) in
      match Map.find t.sizes var with
      | Some prev_size when curr_size >= prev_size -> None
      | None | Some _ -> Some { sizes = Map.set t.sizes ~key:var ~data:curr_size }
    ;;
  end : S)
;;

let check
      ?defaulting
      ?(termination_check = Omniml_options.Termination_check.default)
      ?range
      cst
  =
  let termination_budget_spec =
    let open Omniml_constraint_solver.Termination.Budget.Spec in
    match termination_check with
    | Disabled -> unlimited
    | Threshold n -> bounded_by n
    | Decreasing_instantiations -> decreasing_instantiation_spec
  in
  match
    Omniml_constraint_solver.(solve ?range ?defaulting ~termination_budget_spec cst)
  with
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
                  (type_var : Type.Var.t)
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
       Omniml_error.(raise @@ rigid_variable_escape ~range:(get_range range))
     | Resolution_termination_check_failed ->
       Omniml_error.(
         raise @@ resolution_termination_check_failed ~range:(get_range range)))
;;
