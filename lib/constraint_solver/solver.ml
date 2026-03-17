open! Import
module G = Generalization
module State = G.State

module Error = struct
  type t =
    { it : desc
    ; range : Range.t option
    }

  and desc =
    | Unsatisfiable of Omniml_error.t
    | Unbound_type_var of Type.Var.t
    | Unbound_var of Constraint.Var.t
    | Rigid_variable_escape
    | Cannot_unify of Decoded_type.t * Decoded_type.t
    | Cannot_discharge_match_constraints of Omniml_error.t list
  [@@deriving sexp]

  exception T of t

  let create ~range it = { it; range }
  let raise ~range it = raise @@ T { it; range }
end

module Env = struct
  type t =
    { type_vars : G.Type.t Type.Var.Map.t
    ; expr_vars : G.Scheme.t Constraint.Var.Map.t
    ; curr_region : G.Region.t
    ; range : Range.t option
    }
  [@@deriving sexp_of]

  let raise t err = Error.raise ~range:t.range err
  let with_range t ~range = { t with range = Some range }

  let empty ~range ~curr_region =
    { type_vars = Type.Var.Map.empty
    ; expr_vars = Constraint.Var.Map.empty
    ; curr_region
    ; range
    }
  ;;

  let bind_type_var t ~var ~type_ =
    { t with type_vars = Map.set t.type_vars ~key:var ~data:type_ }
  ;;

  let bind_var t ~var ~type_ =
    { t with expr_vars = Map.set t.expr_vars ~key:var ~data:type_ }
  ;;

  let find_type_var t type_var =
    try Map.find_exn t.type_vars type_var with
    | _ -> raise t @@ Unbound_type_var type_var
  ;;

  let find_var t expr_var =
    try Map.find_exn t.expr_vars expr_var with
    | _ -> raise t @@ Unbound_var expr_var
  ;;

  let enter_new_region ~state t =
    { t with
      curr_region =
        G.new_region ~state t.curr_region ~raise_scope_escape:(fun _type ->
          raise t @@ Rigid_variable_escape)
    }
  ;;

  let create_scheme t root = G.create_scheme ~curr_region:t.curr_region root

  let of_gclosure
        (gclosure : G.Suspended_match.closure)
        ~closure:({ type_vars; vars } : Constraint.Closure.t)
        ~range
        ~curr_region
    =
    let type_vars =
      List.zip_exn type_vars gclosure.variables |> Type.Var.Map.of_alist_exn
    in
    let expr_vars =
      List.zip_exn vars gclosure.schemes |> Constraint.Var.Map.of_alist_exn
    in
    { (empty ~range ~curr_region) with type_vars; expr_vars }
  ;;

  let prev_region t =
    match t.curr_region.parent with
    | None -> t.curr_region
    | Some parent -> parent
  ;;
end

let rec gtype_of_type : state:State.t -> env:Env.t -> Type.t -> G.Type.t =
  fun ~state ~env type_ ->
  let self = gtype_of_type ~state ~env in
  let gformer ~(env : Env.t) args shape =
    let curr_region = env.curr_region in
    G.create_former ~state ~curr_region { args; shape }
  in
  match type_ with
  | Var type_var -> Env.find_type_var env type_var
  | Arrow (t1, t2) -> gformer ~env [ self t1; self t2 ] Sh_arrow
  | Tuple ts -> gformer ~env (List.map ~f:self ts) (Sh_tuple (List.length ts))
  | Constr (ts, ident) ->
    gformer ~env (List.map ~f:self ts) (Sh_constr (List.length ts, ident))
  | Shape (ts, shape) -> gformer ~env (List.map ~f:self ts) shape
  | Poly scheme ->
    let ts, poly_shape = Principal_shape.poly_shape_decomposition_of_scheme scheme in
    gformer ~env (List.map ~f:self ts) (Sh_poly poly_shape)
;;

let unify ~(state : State.t) ~(env : Env.t) gtype1 gtype2 =
  [%log.global.debug
    "Unify" (state : State.t) (env : Env.t) (gtype1 : G.Type.t) (gtype2 : G.Type.t)];
  try
    G.unify ~state ~curr_region:env.curr_region gtype1 gtype2;
    [%log.global.debug
      "(Unify) Running scheduler"
        (gtype1 : G.Type.t)
        (gtype2 : G.Type.t)
        (Scheduler.t () : Scheduler.t)];
    Scheduler.(run (t ()))
  with
  | G.Unify.Unify (gtype1, gtype2) ->
    let decoder = Decoded_type.Decoder.create () in
    (* The let bindings here are to used to ensure order.
       The first type will have the 'newest' allocated variables *)
    let dtype1 = decoder gtype1 in
    let dtype2 = decoder gtype2 in
    Env.raise env (Cannot_unify (dtype1, dtype2))
;;

let forall ~(state : State.t) ~env ~type_var =
  Env.bind_type_var
    env
    ~var:type_var
    ~type_:(G.create_rigid_var ~state ~curr_region:env.curr_region ())
;;

let forall_many ~state ~env type_vars =
  List.fold type_vars ~init:env ~f:(fun env type_var -> forall ~state ~env ~type_var)
;;

let exists ~(state : State.t) ~env ~type_var =
  Env.bind_type_var
    env
    ~var:type_var
    ~type_:(G.create_var ~state ~curr_region:env.curr_region ())
;;

let match_type
  : env:Env.t -> shape:Principal_shape.t -> args:G.Type.t list -> Env.t * Type.Matchee.t
  =
  fun ~env ~shape ~args ->
  let shape_quantifiers = Principal_shape.quantifiers shape in
  let env =
    List.fold2_exn shape_quantifiers args ~init:env ~f:(fun env quantifier arg ->
      Env.bind_type_var env ~var:quantifier ~type_:arg)
  in
  match shape with
  | Sh_arrow ->
    (match shape_quantifiers with
     | [ var1; var2 ] -> env, Arrow (var1, var2)
     | _ -> assert false)
  | Sh_tuple _n -> env, Tuple shape_quantifiers
  | Sh_constr (_n, ident) -> env, Constr (shape_quantifiers, ident)
  | Sh_poly poly_shape -> env, Poly poly_shape.scheme
;;

let rec solve : state:State.t -> env:Env.t -> Constraint.t -> unit =
  fun ~state ~env cst ->
  [%log.global.debug
    "Solving constraint" (state : State.t) (env : Env.t) (cst : Constraint.t)];
  let self ~state ?(env = env) cst = solve ~state ~env cst in
  match cst with
  | True -> ()
  | False err -> Env.raise env @@ Unsatisfiable err
  | Conj (cst1, cst2) ->
    [%log.global.debug "Solving conj lhs"];
    self ~state cst1;
    [%log.global.debug "Solving conj rhs"];
    self ~state cst2
  | Eq (type1, type2) ->
    [%log.global.debug "Decoding type1" (type1 : Type.t)];
    let gtype1 = gtype_of_type ~state ~env type1 in
    [%log.global.debug "Decoded type1" (gtype1 : G.Type.t)];
    [%log.global.debug "Decoding type2" (type2 : Type.t)];
    let gtype2 = gtype_of_type ~state ~env type2 in
    [%log.global.debug "Decoded type2" (gtype2 : G.Type.t)];
    unify ~state ~env gtype1 gtype2
  | Let (let_binding, in_) ->
    [%log.global.debug "Solving let binding"];
    let gbindings = solve_let_binding ~state ~env let_binding in
    let env =
      List.fold gbindings ~init:env ~f:(fun env (var, gscheme) ->
        Env.bind_var env ~var ~type_:gscheme)
    in
    [%log.global.debug "Solving let body"];
    self ~state ~env in_
  | Instance (var, expected_type) ->
    [%log.global.debug "Decoding expected_type" (expected_type : Type.t)];
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    [%log.global.debug "Decoded expected_type" (expected_gtype : G.Type.t)];
    let var_gscheme = Env.find_var env var in
    [%log.global.debug
      "Instantiating scheme" (var : Constraint.Var.t) (var_gscheme : G.Scheme.t)];
    let actual_gtype = G.instantiate ~state ~curr_region:env.curr_region var_gscheme in
    [%log.global.debug "Scheme instance" (actual_gtype : G.Type.t)];
    unify ~state ~env actual_gtype expected_gtype
  | Exists (type_var, cst) ->
    [%log.global.debug "Binding unification for type_var" (type_var : Type.Var.t)];
    let env = exists ~state ~env ~type_var in
    [%log.global.debug "Updated env" (env : Env.t)];
    [%log.global.debug "Solving exist body"];
    self ~state ~env cst
  | Forall (type_vars, in_) ->
    let env = Env.enter_new_region ~state env in
    let env = forall_many ~state ~env type_vars in
    self ~state ~env in_
  | Match { matchee; closure; case; else_; error } ->
    let gmatchee = Env.find_type_var env matchee in
    [%log.global.debug "Matchee type" (gmatchee : G.Type.t)];
    let gclosure = gclosure_of_closure ~env closure in
    [%log.global.debug
      "Closure of suspended match" (gclosure : G.Suspended_match.closure)];
    (* Register match for the shape *)
    let case ~curr_region ~shape ~args =
      [%log.global.debug "Entered match handler" (shape : Principal_shape.t)];
      (* Enter region and construct env *)
      let env = Env.of_gclosure gclosure ~closure ~curr_region ~range:env.range in
      [%log.global.debug "Handler env" (env : Env.t)];
      [%log.global.debug "Handler state" (state : State.t)];
      (* Solve *)
      let env, matchee = match_type ~env ~shape ~args in
      [%log.global.debug
        "Matchee and updated env" (matchee : Type.Matchee.t) (env : Env.t)];
      let cst = case matchee in
      [%log.global.debug "Generated constraint from case" (cst : Constraint.t)];
      solve ~state ~env cst;
      [%log.global.debug "Solved generated constraint" (cst : Constraint.t)];
      [%log.global.debug "Exiting case region"]
    in
    let else_ () =
      [%log.global.debug "Entered match default handler"];
      else_ ()
    in
    [%log.global.debug "Suspending match..."];
    G.Suspended_match.match_or_yield
      ~state
      ~curr_region:env.curr_region
      { matchee = gmatchee; closure = gclosure; case; else_; error }
  | With_range (t, range) -> solve ~state ~env:(Env.with_range env ~range) t

and solve_let_binding ~state ~env { type_vars; in_; bindings } =
  let env = Env.enter_new_region ~state env in
  [%log.global.debug "Entered new region" (env : Env.t)];
  let env =
    List.fold type_vars ~init:env ~f:(fun env (flex, type_var) ->
      match flex with
      | Flexible -> exists ~state ~env ~type_var
      | Rigid -> forall ~state ~env ~type_var)
  in
  [%log.global.debug
    "Bound type vars"
      (type_vars : (Constraint.flexibility * Type.Var.t) list)
      (env : Env.t)];
  [%log.global.debug "Solving scheme's constraint"];
  solve ~state ~env in_;
  let gbindings =
    List.map bindings ~f:(fun { binding_var; binding_type } ->
      [%log.global.debug
        "Decoding binding type" (binding_var : Constraint.Var.t) (binding_type : Type.t)];
      let binding_gtype = gtype_of_type ~state ~env binding_type in
      [%log.global.debug
        "Type of binding" (binding_var : Constraint.Var.t) (binding_gtype : G.Type.t)];
      let gscheme = Env.create_scheme env binding_gtype in
      binding_var, gscheme)
  in
  [%log.global.debug "Bindings" (gbindings : (Constraint.Var.t * G.Scheme.t) list)];
  gbindings

and gclosure_of_closure ~env closure : G.Suspended_match.closure =
  let variables = List.map closure.type_vars ~f:(Env.find_type_var env) in
  let schemes = List.map closure.vars ~f:(Env.find_var env) in
  { variables; schemes }
;;

let solve
  :  ?range:Range.t
  -> ?defaulting:Omniml_options.Defaulting.t
  -> Constraint.t
  -> (unit, Error.t) result
  =
  fun ?range ?defaulting cst ->
  try
    Scheduler.(clear (t ()));
    let state = State.create ?defaulting () in
    let root_region = State.root_region state in
    let env = Env.empty ~curr_region:root_region ~range in
    [%log.global.debug "Initial env and state" (state : State.t) (env : Env.t)];
    solve ~state ~env cst;
    [%log.global.debug "State" (state : State.t)];
    [%log.global.debug "Generalizing root region" (env.curr_region : G.Region.t)];
    G.Region.mark ~state env.curr_region;
    let shape_var_errors =
      G.force_root_generalization_and_return_unsolved_shape_var_errors ~state
    in
    [%log.global.debug "Generalized root region" (env.curr_region : G.Region.t)];
    if not Scheduler.(is_empty (t ()))
    then raise_bug_s ~here:[%here] [%message "Scheduler not flushed"];
    (* No more regions to generalize *)
    if not (Tree.With_dirty.is_empty state.region_tree)
    then
      raise_bug_s
        ~here:[%here]
        [%message
          "Region tree is not empty"
            (state.region_tree : G.Type.t G.Pool.t Tree.With_dirty.t)];
    if not (List.is_empty shape_var_errors)
    then Error.raise ~range:None @@ Cannot_discharge_match_constraints shape_var_errors;
    (* If we have no remaining shape var errors, then it must be the case that we have 
       no alive regions. *)
    let num_type_partially_generalized_regions = State.num_alive_regions state in
    let num_shape_partially_generalized_regions =
      Principal_shape.Var.State.num_partially_generalized_regions state.shape_var_state
    in
    if
      num_shape_partially_generalized_regions > 0
      || num_type_partially_generalized_regions > 0
    then
      raise_bug_s
        ~here:[%here]
        [%message
          "Residual suspended constraints!"
            (num_shape_partially_generalized_regions : int)
            (num_type_partially_generalized_regions : int)
            (state : State.t)];
    Ok ()
  with
  (* Catch solver exceptions *)
  | G.Unify.Unify (gtype1, gtype2) ->
    let decoder = Decoded_type.Decoder.create () in
    (* The let bindings here are to used to ensure order.
       The first type will have the 'newest' allocated variables *)
    let dtype1 = decoder gtype1 in
    let dtype2 = decoder gtype2 in
    Error (Error.create ~range (Cannot_unify (dtype1, dtype2)))
  | G.Suspended_match.Cannot_match_on_rigid report
  | G.Suspended_match.Inconsistent_defaults report ->
    (* Catch suspended match constraint exceptions *)
    Error (Error.create ~range (Unsatisfiable report))
  | Error.T err -> Error err
;;
