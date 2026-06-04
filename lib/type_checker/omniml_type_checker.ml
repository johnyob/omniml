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

module Decreasing_instantiation_check = struct
  open Omniml_constraint_solver
  open Termination

  module Ordinal = struct
    module T = struct
      type t =
        { omega_coeff : int
        ; one_coeff : int
        }
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)

    let of_int n = { one_coeff = n; omega_coeff = 0 }
    let zero = of_int 0
    let one = of_int 1
    let omega = { omega_coeff = 1; one_coeff = 0 }

    let ( + ) t1 t2 =
      { one_coeff = t1.one_coeff + t2.one_coeff
      ; omega_coeff = t1.omega_coeff + t2.omega_coeff
      }
    ;;
  end

  module Size = struct
    let rec of_decoded_type (decoded_type : Decoded_type.t) =
      match decoded_type with
      | Var _ | Mu _ -> Ordinal.omega
      | App (decoded_types, _) ->
        Ordinal.(one + List.sum (module Ordinal) decoded_types ~f:of_decoded_type)
    ;;

    let of_instantiation decoded_types =
      decoded_types |> List.sum (module Ordinal) ~f:of_decoded_type
    ;;
  end

  let reject_if_increasing_instantiation witness =
    let open Witness in
    let open Elab.Let_syntax in
    let instantiation_size_table = Hashtbl.create (module Constraint.Var) in
    let rec loop witness =
      match Witness.view witness with
      | Hole -> return false
      | Spine s ->
        let%bind instantiation = Spine.instantiation s in
        let curr_size = Size.of_instantiation instantiation in
        let head = Spine.head s in
        if
          match Hashtbl.find instantiation_size_table head with
          | None -> true
          | Some prev_size -> Ordinal.(curr_size < prev_size)
        then (
          Hashtbl.set instantiation_size_table ~key:head ~data:curr_size;
          loop_args (Spine.args s))
        else
          (* [curr_size >= prev_size], therefore reject witness *)
          return true
    and loop_args = function
      | [] -> return false
      | witness :: witnesses ->
        let%bind reject = loop witness in
        if reject then return true else loop_args witnesses
    in
    loop witness
  ;;

  let v : Check.t =
    { recursive_occurrence_threshold = 256; rejects = reject_if_increasing_instantiation }
  ;;
end

let check
      ?defaulting
      ?(termination_check = Omniml_options.Termination_check.default)
      ?range
      cst
  =
  let termination_check =
    match termination_check with
    | Disabled -> Omniml_constraint_solver.Termination.Check.disabled
    | Threshold n -> Omniml_constraint_solver.Termination.Check.threshold n
    | Decreasing_instantiations -> Decreasing_instantiation_check.v
  in
  match Omniml_constraint_solver.(solve ?range ?defaulting ~termination_check cst) with
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
