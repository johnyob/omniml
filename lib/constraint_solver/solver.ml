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
    | Resolution_termination_check_failed
  [@@deriving sexp]

  exception T of t

  let create ?range it = { it; range }
  let raise ?range it = raise @@ T { it; range }
end

module Termination = struct
  module Budget = struct
    module Spec = struct
      module type S = sig
        type t [@@deriving sexp_of, compare]

        val initial : t
        val consume : Constraint.Var.t -> Decoded_type.t Lazy.t -> t -> t option
      end

      type t = ((module S)[@sexp.opaque]) [@@deriving sexp_of]

      let unlimited : t =
        (module struct
          type t = unit [@@deriving sexp_of, compare]

          let initial = ()
          let consume _var _type _t = Some ()
        end)
      ;;

      let bounded_by n : t =
        (module struct
          type t = int [@@deriving sexp_of, compare]

          let initial = n
          let consume _var _type t = if t = 0 then None else Some (t - 1)
        end)
      ;;
    end

    type t =
      | Initial
      | Consume of Constraint.Var.t * G.Type.t * t
    [@@deriving sexp_of]

    let initial = Initial
    let consume var type_ t = Consume (var, type_, t)
  end

  module Check = struct
    module Pressure = struct
      (* To decide when the solver should check the termination budget, 
         we track a heuristic "peassure": the number of times a variable 
         has been seen recursively. 

         If this count exceeds a threshold, the budget is evaluated 
         eargerly; otherwise evaluation is delayed until after constraint 
         solving. *)

      type t = { recursive_occurrences : int Constraint.Var.Map.t } [@@deriving sexp_of]

      let empty = { recursive_occurrences = Constraint.Var.Map.empty }

      let increase t var =
        { recursive_occurrences =
            Map.update
              t.recursive_occurrences
              var
              ~f:(Option.value_map ~default:1 ~f:Int.succ)
        }
      ;;

      let reset t var =
        { recursive_occurrences = Map.set t.recursive_occurrences ~key:var ~data:0 }
      ;;

      let get t var =
        try Map.find_exn t.recursive_occurrences var with
        | _ -> 0
      ;;

      let threshold = 256
    end

    type t =
      { budget_spec : Budget.Spec.t
      ; mutable delayed_checks : (Range.t option * Budget.t) list
      }
    [@@deriving sexp_of]

    let create budget_spec = { budget_spec; delayed_checks = [] }

    let raise_if_exceeds t ?range budget =
      let (module Tbs) = t.budget_spec in
      let decode_type type_ =
        lazy
          (let dec = Decoded_type.Decoder.create () in
           dec type_)
      in
      let rec interpret = function
        | Budget.Initial -> Tbs.initial
        | Consume (var, type_, budget_term) ->
          let budget = interpret budget_term in
          (match Tbs.consume var (decode_type type_) budget with
           | Some budget -> budget
           | None -> Error.(raise ?range @@ Resolution_termination_check_failed))
      in
      ignore (interpret budget : Tbs.t)
    ;;

    let add_delayed_check t ?range termination_budget =
      t.delayed_checks <- (range, termination_budget) :: t.delayed_checks
    ;;

    let force_delayed_checks t =
      List.iter t.delayed_checks ~f:(fun (range, budget) ->
        raise_if_exceeds t ?range budget)
    ;;
  end
end

module Env = struct
  type t =
    { type_vars : G.Type.t Type.Var.Map.t
    ; expr_vars : G.Scheme.t Constraint.Var.Map.t
    ; curr_region : G.Region.t
    ; range : Range.t option
    ; termination_check : Termination.Check.t
    ; termination_pressure : Termination.Check.Pressure.t
    }
  [@@deriving sexp_of]

  let raise t err = Error.raise ?range:t.range err
  let with_range t ~range = { t with range = Some range }

  let empty ~range ~curr_region ~termination_check =
    { type_vars = Type.Var.Map.empty
    ; expr_vars = Constraint.Var.Map.empty
    ; curr_region
    ; range
    ; termination_check
    ; termination_pressure = Termination.Check.Pressure.empty
    }
  ;;

  let bind_type_var t ~var ~type_ =
    { t with type_vars = Map.set t.type_vars ~key:var ~data:type_ }
  ;;

  let bind_var t ~var ~type_ =
    { t with expr_vars = Map.set t.expr_vars ~key:var ~data:type_ }
  ;;

  let increase_termination_pressure t var =
    { t with
      termination_pressure =
        Termination.Check.Pressure.increase t.termination_pressure var
    }
  ;;

  let reset_termination_pressure t var =
    { t with
      termination_pressure = Termination.Check.Pressure.reset t.termination_pressure var
    }
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
        ~termination_check
    =
    let type_vars =
      List.zip_exn type_vars gclosure.variables |> Type.Var.Map.of_alist_exn
    in
    let expr_vars =
      List.zip_exn vars gclosure.schemes |> Constraint.Var.Map.of_alist_exn
    in
    { (empty ~range ~curr_region ~termination_check) with type_vars; expr_vars }
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
  | Implicit_arrow (t1, t2) -> gformer ~env [ self t1; self t2 ] Sh_implicit_arrow
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
  | Sh_implicit_arrow ->
    (match shape_quantifiers with
     | [ var1; var2 ] -> env, Implicit_arrow (var1, var2)
     | _ -> assert false)
  | Sh_tuple _n -> env, Tuple shape_quantifiers
  | Sh_constr (_n, ident) -> env, Constr (shape_quantifiers, ident)
  | Sh_poly poly_shape -> env, Poly poly_shape.scheme
;;

module Implicit_resolution = struct
  (* Implicit resolution via multi-stage filtering:

     Stage 1: Strip implicit parameter types from expected type and all candidates,
              tracking the spine depth (number of implicit arrows stripped)
     Stage 2: Eliminate candidates by comparing simple shapes (non-implicit-arrow shapes)

     A simple shape is any shape that is not Sh_implicit_arrow.

     The spine depth tracks:
     - For expected type: number of implicit arrows stripped
     - For each candidate: difference in implicit arrow count (for eta-expansion/holes)
  *)

  (** Shared error state for implicit resolution.
      Ensures only a single error is raised even when multiple match operations fail. *)
  module Error_switch = struct
    type t =
      { mutable has_raised_error : bool
      ; range : Range.t
      }

    let create ~range = { has_raised_error = false; range }
    let omniml_error t = Omniml_error.ambiguous_overloading ~range:t.range

    let raise t () =
      t.has_raised_error <- true;
      Error.raise ~range:t.range @@ Unsatisfiable (omniml_error t)
    ;;

    let omniml_error_if_not_raised t =
      if t.has_raised_error
      then None
      else (
        t.has_raised_error <- true;
        Some (omniml_error t))
    ;;

    let match_error t : Constraint.Match_error.t -> Omniml_error.t option = function
      | Inconsistent_default _ -> assert false
      | Cannot_default | Matchee_is_rigid -> omniml_error_if_not_raised t
    ;;

    let match_ t ~state ~curr_region matchee ~closure ~with_ =
      G.Suspended_match.match_or_yield
        ~state
        ~curr_region
        { matchee; closure; case = with_; else_ = raise t; error = match_error t }
    ;;
  end

  module Filter_switch : sig
    type 'a t [@@deriving sexp_of]

    val create : (Constraint.Var.t * 'a) list -> 'a t
    val remove : 'a t -> Constraint.Var.t -> unit
    val mem : 'a t -> Constraint.Var.t -> bool

    val add_handler
      :  'a t
      -> Constraint.Var.t
      -> Principal_shape.Var.Registered_handler.t
      -> unit

    exception Not_resolved

    val try_resolve : 'a t -> f:(Constraint.Var.t -> 'a -> 'b) -> 'b
  end = struct
    module Candidate = struct
      type 'a t =
        { it : 'a
        ; mutable handlers : (Principal_shape.Var.Registered_handler.t list[@sexp.opaque])
        }
      [@@deriving sexp_of]

      let create it = { it; handlers = [] }
      let add_handler t handler = t.handlers <- handler :: t.handlers

      let cancel_handlers t =
        List.iter t.handlers ~f:Principal_shape.Var.Registered_handler.cancel_if_pending
      ;;
    end

    type 'a t = { remaining_candidates : (Constraint.Var.t, 'a Candidate.t) Hashtbl.t }
    [@@deriving sexp_of]

    let create candidates =
      let candidates =
        List.map candidates ~f:(fun (var, it) -> var, Candidate.create it)
        |> Hashtbl.of_alist_exn (module Constraint.Var)
      in
      assert (Hashtbl.length candidates >= 2);
      { remaining_candidates = candidates }
    ;;

    let add_handler t candidate_var handler =
      let candidate = Hashtbl.find_exn t.remaining_candidates candidate_var in
      Candidate.add_handler candidate handler
    ;;

    let mem t candidate_var = Hashtbl.mem t.remaining_candidates candidate_var

    let remove t candidate_var =
      Hashtbl.find_and_remove t.remaining_candidates candidate_var
      |> Option.iter ~f:Candidate.cancel_handlers
    ;;

    exception Not_resolved

    let try_resolve t ~f =
      if Hashtbl.length t.remaining_candidates <> 1
      then raise Not_resolved
      else (
        match Hashtbl.to_alist t.remaining_candidates with
        | [ (candidate_var, candidate) ] ->
          Candidate.cancel_handlers candidate;
          f candidate_var candidate.it
        | _ -> assert false)
    ;;
  end

  let match_possibly_generic ~error_switch ~state ~curr_region matchee ~closure ~with_ =
    if
      G.Type.is_generic matchee
      &&
      match G.Type.inner matchee with
      | Var -> true
      | _ -> false
    then None
    else Error_switch.match_ error_switch ~state ~curr_region matchee ~closure ~with_
  ;;

  let strip_implicits
        ~(state : State.t)
        ~(error_switch : Error_switch.t)
        ~curr_region
        (type_ : G.Type.t)
        ~closure
        ~(with_ : spine:int -> ret:G.Type.t -> unit)
    : unit
    =
    let rec loop type_ ~spine =
      match_possibly_generic
        ~error_switch
        ~state
        ~curr_region
        type_
        ~closure
        ~with_:(fun ~shape ~args ->
          match shape with
          | Sh_implicit_arrow ->
            (match args with
             | [ _implicit_param; result_type ] -> loop result_type ~spine:(spine + 1)
             | _ -> assert false)
          | _simple_shape -> with_ ~spine ~ret:type_)
      |> ignore
    in
    loop type_ ~spine:0
  ;;

  let candidate_match
        ~(state : State.t)
        ~(error_switch : Error_switch.t)
        ~(filter_switch : 'a Filter_switch.t)
        ~curr_region
        ~candidate
        matchee
        ~closure
        ~with_
    =
    if Filter_switch.mem filter_switch candidate
    then
      match_possibly_generic ~error_switch ~state ~curr_region matchee ~closure ~with_
      |> Option.iter ~f:(Filter_switch.add_handler filter_switch candidate)
  ;;

  let remove_and_try_resolve ~filter_switch candidate ~with_ =
    Filter_switch.remove filter_switch candidate;
    try Filter_switch.try_resolve filter_switch ~f:with_ with
    | Filter_switch.Not_resolved -> ()
  ;;

  let rec filter_by_shape
            ~(state : State.t)
            ~(env : Env.t)
            ~error_switch
            ~filter_switch
            ~candidate
            ~candidate_region
            expected_type_p
            candidate_type_p
            ~closure
            ~with_
    =
    candidate_match
      ~state
      ~error_switch
      ~filter_switch
      ~curr_region:env.curr_region
      ~candidate
      expected_type_p
      ~closure
      ~with_:(fun ~shape:expected_type_p_shape ~args:expected_type_p_args ->
        candidate_match
          ~state
          ~error_switch
          ~filter_switch
          ~curr_region:candidate_region
          ~candidate
          candidate_type_p
          ~closure:{ closure with variables = expected_type_p_args @ closure.variables }
          ~with_:(fun ~shape:candidate_type_p_shape ~args:candidate_type_p_args ->
            compare_shapes_and_filter
              ~state
              ~env
              ~error_switch
              ~filter_switch
              ~candidate
              ~candidate_region
              expected_type_p_shape
              expected_type_p_args
              candidate_type_p_shape
              candidate_type_p_args
              ~with_
              ~closure))

  and compare_shapes_and_filter
        ~(state : State.t)
        ~(env : Env.t)
        ~error_switch
        ~filter_switch
        ~candidate
        ~candidate_region
        expected_type_p_shape
        expected_type_p_args
        candidate_type_p_shape
        candidate_type_p_args
        ~closure
        ~with_
    =
    if Principal_shape.equal expected_type_p_shape candidate_type_p_shape
    then
      List.iter2_exn
        expected_type_p_args
        candidate_type_p_args
        ~f:(fun expected_arg over_arg ->
          filter_by_shape
            ~state
            ~env
            ~error_switch
            ~filter_switch
            ~candidate
            ~candidate_region
            expected_arg
            over_arg
            ~with_
            ~closure)
    else remove_and_try_resolve ~filter_switch candidate ~with_
  ;;

  let match_ ~(state : State.t) ~(env : Env.t) ~candidates expected_type ~with_ =
    let error_switch =
      Error_switch.create ~range:(Option.value_exn ~here:[%here] env.range)
    in
    let closure : G.Suspended_match.closure =
      { variables = [ expected_type ]; schemes = List.map ~f:snd candidates }
    in
    let strip_implicits_for_candidates candidates ~with_ =
      let rec loop acc = function
        | [] -> with_ (List.rev acc)
        | (candidate_var, candidate_scheme) :: candidates ->
          strip_implicits
            ~state
            ~error_switch
            ~curr_region:candidate_scheme.G.Scheme.region
            candidate_scheme.root
            ~closure
            ~with_:(fun ~spine ~ret ->
              loop ((candidate_var, candidate_scheme, spine, ret) :: acc) candidates)
      in
      loop [] candidates
    in
    (* Step 1: Obtain the spine lengths of all implicit arrows. *)
    strip_implicits
      ~state
      ~error_switch
      ~curr_region:env.curr_region
      expected_type
      ~closure
      ~with_:(fun ~spine:expected_spine ~ret:expected_type_ret ->
        let with_ candidate_var (candidate_scheme, candidate_spine) =
          if candidate_spine < expected_spine
          then Error_switch.raise error_switch ()
          else
            with_
              candidate_var
              ~implicit_spine:(candidate_spine - expected_spine)
              candidate_scheme
        in
        strip_implicits_for_candidates
          candidates
          ~with_:(fun candidates_with_spine_rets ->
            match candidates_with_spine_rets with
            | [] -> Error_switch.raise error_switch ()
            | [ (candidate_var, candidate_scheme, candidate_spine, _candidate_type_ret) ]
              ->
              (* Edge case: Only one candidate, solve immediately *)
              with_ candidate_var (candidate_scheme, candidate_spine)
            | candidates_with_spine_rets ->
              let filter_switch =
                candidates_with_spine_rets
                |> List.map ~f:(fun (x, scm, spine, _ret) -> x, (scm, spine))
                |> Filter_switch.create
              in
              (* Step 2: Filter the candidates by the return types. *)
              List.iter
                candidates_with_spine_rets
                ~f:
                  (fun
                    (candidate_var, candidate_scheme, _candidate_spine, candidate_type_ret)
                  ->
                  filter_by_shape
                    ~state
                    ~env
                    ~error_switch
                    ~filter_switch
                    ~candidate:candidate_var
                    ~candidate_region:candidate_scheme.G.Scheme.region
                    expected_type_ret
                    candidate_type_ret
                    ~closure
                    ~with_)))
  ;;
end

let instantiate ~(state : State.t) ~(env : Env.t) gscheme expected_type =
  let instantiation, actual_type =
    G.instantiate ~state ~curr_region:env.curr_region gscheme
  in
  unify ~state ~env actual_type expected_type;
  instantiation
;;

let rec resolve_implicit
          ~state
          ~env
          ~termination_budget
          ~implicit_scope
          (expected_type : G.Type.t)
  =
  Implicit_resolution.match_
    ~state
    ~env
    ~candidates:implicit_scope
    expected_type
    ~with_:(fun head ~implicit_spine gscheme ->
      let curr_region = env.curr_region in
      (* Reconstruct the implicit spine *)
      let implicits =
        List.init implicit_spine ~f:(fun _ -> G.create_var ~state ~curr_region ())
      in
      let implicit_type =
        List.fold_right implicits ~init:expected_type ~f:(fun implicit ret ->
          G.create_former
            ~state
            ~curr_region
            { shape = Sh_implicit_arrow; args = [ implicit; ret ] })
      in
      (* Instantiate the type scheme *)
      let _instantiation = instantiate ~state ~env gscheme implicit_type in
      (* At this point, implicits contains the types that we must recursively resolve *)
      let termination_budget =
        Termination.Budget.consume head expected_type termination_budget
      in
      match implicits with
      | [] ->
        (* A leaf constraint in resolution. Despite the fact that resolution has 
           terminated, we need to check the termination budget computation after 
           constraint solving finishes. *)
        Termination.Check.add_delayed_check
          env.termination_check
          ?range:env.range
          termination_budget
      | _ :: _ ->
        (* Increase termination pressure for [head] *)
        let env = Env.increase_termination_pressure env head in
        (* Check if pressure threshold has exceeded *)
        let env =
          if Termination.Check.Pressure.(get env.termination_pressure head > threshold)
          then (
            Termination.Check.raise_if_exceeds
              env.termination_check
              ?range:env.range
              termination_budget;
            (* TODO: Reset globally instead of just in current path *)
            Env.reset_termination_pressure env head)
          else env
        in
        List.iter
          implicits
          ~f:(resolve_implicit ~state ~env ~termination_budget ~implicit_scope))
;;

let resolve_toplevel_implicit
      ~state
      ~env
      ~(implicit_scope : Constraint.Implicit_scope.t)
      expected_type
  =
  let implicit_scope =
    implicit_scope.vars
    |> Set.to_list
    |> List.map ~f:(fun var -> var, Env.find_var env var)
  in
  resolve_implicit
    ~state
    ~env
    ~termination_budget:Termination.Budget.initial
    ~implicit_scope
    expected_type
;;

let rec solve : state:State.t -> env:Env.t -> Constraint.t -> unit =
  fun ~state ~env cst ->
  [%log.global.debug
    "Solving constraint" (state : State.t) (env : Env.t) (cst : Constraint.t)];
  let self ~state ~env cst = solve ~state ~env cst in
  match cst with
  | True -> ()
  | False err -> Env.raise env @@ Unsatisfiable err
  | Conj (cst1, cst2) ->
    [%log.global.debug "Solving conj lhs"];
    self ~state ~env cst1;
    [%log.global.debug "Solving conj rhs"];
    self ~state ~env cst2
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
    let (_quantifiers : unit -> G.Type.t list) =
      instantiate ~state ~env var_gscheme expected_gtype
    in
    ()
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
    let case ~shape ~args =
      [%log.global.debug "Entered match handler" (shape : Principal_shape.t)];
      (* Enter region and construct env *)
      let env =
        Env.of_gclosure
          gclosure
          ~closure
          ~curr_region:env.curr_region
          ~range:env.range
          ~termination_check:env.termination_check
      in
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
    let error e = Some (error e) in
    [%log.global.debug "Suspending match..."];
    let _ : Principal_shape.Var.Registered_handler.t option =
      G.Suspended_match.match_or_yield
        ~state
        ~curr_region:env.curr_region
        { matchee = gmatchee; closure = gclosure; case; else_; error }
    in
    ()
  | Implicit (expected_type, implicit_scope) ->
    [%log.global.debug "Decoding expected_type" (expected_type : Type.t)];
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    [%log.global.debug "Decoded expected_type" (expected_gtype : G.Type.t)];
    resolve_toplevel_implicit ~state ~env ~implicit_scope expected_gtype
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
  -> ?termination_budget_spec:Termination.Budget.Spec.t
  -> Constraint.t
  -> (unit, Error.t) result
  =
  fun ?range
    ?defaulting
    ?(termination_budget_spec = Termination.Budget.Spec.unlimited)
    cst ->
  try
    Scheduler.(clear (t ()));
    let state = State.create ?defaulting () in
    let root_region = State.root_region state in
    let termination_check = Termination.Check.create termination_budget_spec in
    let env = Env.empty ~curr_region:root_region ~range ~termination_check in
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
    then Error.raise @@ Cannot_discharge_match_constraints shape_var_errors;
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
    (* Check that all termination budgets *)
    Termination.Check.force_delayed_checks termination_check;
    Ok ()
  with
  (* Catch solver exceptions *)
  | G.Unify.Unify (gtype1, gtype2) ->
    let decoder = Decoded_type.Decoder.create () in
    (* The let bindings here are to used to ensure order.
       The first type will have the 'newest' allocated variables *)
    let dtype1 = decoder gtype1 in
    let dtype2 = decoder gtype2 in
    Error (Error.create ?range (Cannot_unify (dtype1, dtype2)))
  | G.Suspended_match.Cannot_match_on_rigid report
  | G.Suspended_match.Inconsistent_defaults report ->
    (* Catch suspended match constraint exceptions *)
    Error (Error.create ?range (Unsatisfiable report))
  | Error.T err -> Error err
;;
