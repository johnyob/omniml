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

  let create ~range it = { it; range }
  let raise ~range it = raise @@ T { it; range }
end

module Termination = struct
  module History = struct
    type t = { seen : int Constraint.Var.Map.t } [@@deriving sexp_of]

    let empty = { seen = Constraint.Var.Map.empty }

    let mark t var =
      { seen = Map.update t.seen var ~f:(Option.value_map ~default:1 ~f:Int.succ) }
    ;;

    let reset t var = { seen = Map.set t.seen ~key:var ~data:0 }

    let count t var =
      try Map.find_exn t.seen var with
      | _ -> 0
    ;;
  end

  module Witness = struct
    module Elab = struct
      module T = struct
        type 'a t = Decoded_type.Decoder.t -> 'a

        let return x _dec = x
        let bind t ~f = fun dec -> f (t dec) dec
        let map = `Define_using_bind
      end

      include T
      include Monad.Make (T)

      let decode_type type_ : Decoded_type.t t = fun dec -> dec type_
      let run t = t (Decoded_type.Decoder.create ())
    end

    module T = struct
      type t = desc ref

      and desc =
        | Hole
        | Spine of spine

      and spine =
        { head : Constraint.Var.t
        ; instantiation : unit -> G.Type.t list
        ; args : t list
        }
      [@@deriving sexp_of]
    end

    include T

    module Spine = struct
      type t = T.spine [@@deriving sexp_of]

      let create ~head ~instantiation ~args = { head; instantiation; args }
      let head t = t.head
      let instantiation t = t.instantiation () |> List.map ~f:Elab.decode_type |> Elab.all
      let args t = t.args
    end

    type view = desc =
      | Hole
      | Spine of spine
    [@@deriving sexp_of]

    let view (t : t) = !t
    let hole () = ref Hole
    let spine spine = ref (Spine spine)
  end

  module Check = struct
    type t =
      { recursive_occurrence_threshold : int
      ; rejects : Witness.t -> bool Witness.Elab.t
      }
    [@@deriving sexp_of]

    let trigger_check t history ~on =
      History.count history on > t.recursive_occurrence_threshold
    ;;

    let check_if_exceeded_threshold t history ~on ~root_witness =
      if trigger_check t history ~on
      then `Check (t.rejects root_witness |> Witness.Elab.run)
      else `No_check
    ;;

    let disabled =
      { recursive_occurrence_threshold = Int.max_value
      ; rejects = (fun _ -> Witness.Elab.return false)
      }
    ;;

    let threshold n =
      { recursive_occurrence_threshold = n
      ; rejects = (fun _ -> Witness.Elab.return true)
      }
    ;;
  end
end

module Env = struct
  type t =
    { type_vars : G.Type.t Type.Var.Map.t
    ; expr_vars : G.Scheme.t Constraint.Var.Map.t
    ; implicit_vars : Constraint.Var.Set.t
    ; curr_region : G.Region.t
    ; range : Range.t option
    ; termination_history : Termination.History.t
    ; termination_check : Termination.Check.t
    }
  [@@deriving sexp_of]

  let raise t err = Error.raise ~range:t.range err
  let with_range t ~range = { t with range = Some range }

  let empty ~range ~curr_region ~termination_check =
    { type_vars = Type.Var.Map.empty
    ; expr_vars = Constraint.Var.Map.empty
    ; implicit_vars = Constraint.Var.Set.empty
    ; curr_region
    ; range
    ; termination_history = Termination.History.empty
    ; termination_check
    }
  ;;

  let bind_type_var t ~var ~type_ =
    { t with type_vars = Map.set t.type_vars ~key:var ~data:type_ }
  ;;

  let bind_var t ~var ~type_ =
    { t with expr_vars = Map.set t.expr_vars ~key:var ~data:type_ }
  ;;

  let add_implicit_var t var = { t with implicit_vars = Set.add t.implicit_vars var }

  let mark_var_in_termination_history t var =
    { t with termination_history = Termination.History.mark t.termination_history var }
  ;;

  let reset_var_in_termination_history t var =
    { t with termination_history = Termination.History.reset t.termination_history var }
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

  let create_scheme t ?implicits root =
    G.create_scheme ~curr_region:t.curr_region ?implicits root
  ;;

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

  let implicit_env t =
    t.implicit_vars |> Set.to_list |> List.map ~f:(fun var -> var, find_var t var)
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

module Overload = struct
  module Switch : sig
    type t [@@deriving sexp_of]

    val create : (Constraint.Var.t * G.Scheme.t) list -> t

    val match_
      :  state:State.t
      -> env:Env.t
      -> in_:t
      -> curr_region:[ `Env | `Over ]
      -> over:Constraint.Var.t
      -> G.Type.t
      -> closure:G.Type.t list
      -> with_:(shape:Principal_shape.t -> args:G.Type.t list -> unit)
      -> unit

    val remove : t -> Constraint.Var.t -> unit

    exception Not_resolved

    val try_resolve : t -> f:(Constraint.Var.t -> G.Scheme.t -> 'a) -> 'a
    val iter : t -> f:(key:Constraint.Var.t -> data:G.Scheme.t -> unit) -> unit
  end = struct
    type t =
      { mutable remaining_overs : over Constraint.Var.Map.t
      ; mutable has_raised_error : bool
      }

    and over =
      { mutable over_handlers :
          (Principal_shape.Var.Registered_handler.t list[@sexp.opaque])
      ; over_scheme : G.Scheme.t
      }
    [@@deriving sexp_of]

    let add_handler_over over handler =
      over.over_handlers <- handler :: over.over_handlers
    ;;

    let cancel_over over =
      List.iter
        over.over_handlers
        ~f:Principal_shape.Var.Registered_handler.cancel_if_pending
    ;;

    let error t ~range =
      if t.has_raised_error
      then None
      else (
        t.has_raised_error <- true;
        Some Omniml_error.(ambiguous_overloading ~range))
    ;;

    let create initial_overs =
      { remaining_overs =
          initial_overs
          |> List.map ~f:(fun (over_var, over_scheme) ->
            over_var, { over_scheme; over_handlers = [] })
          |> Constraint.Var.Map.of_alist_exn
      ; has_raised_error = false
      }
    ;;

    let remove t over_var =
      Map.find t.remaining_overs over_var
      |> Option.iter ~f:(fun over ->
        cancel_over over;
        t.remaining_overs <- Map.remove t.remaining_overs over_var)
    ;;

    exception Not_resolved

    let try_resolve t ~f =
      if Map.length t.remaining_overs <> 1
      then raise Not_resolved
      else (
        match Map.to_alist t.remaining_overs with
        | [ (over_var, over) ] ->
          cancel_over over;
          f over_var over.over_scheme
        | _ -> assert false)
    ;;

    let match_
          ~(state : State.t)
          ~(env : Env.t)
          ~(in_ : t)
          ~curr_region
          ~over
          matchee
          ~closure
          ~with_
      =
      let range = Option.value_exn ~here:[%here] env.range in
      match Map.find in_.remaining_overs over with
      | None ->
        (* overload was already removed, no need to generate any subsequent constraints *)
        ()
      | Some over ->
        let curr_region =
          match curr_region with
          | `Env -> env.curr_region
          | `Over -> over.over_scheme.region
        in
        G.Suspended_match.match_or_yield
          ~state
          ~curr_region
          { matchee
          ; closure = { variables = closure; schemes = [ over.over_scheme ] }
          ; case = with_
          ; else_ = (fun () -> Omniml_error.(raise @@ ambiguous_overloading ~range))
          ; error =
              (function
                | Cannot_default -> error in_ ~range
                | Inconsistent_default _ -> assert false
                | Matchee_is_rigid -> Some Omniml_error.(ambiguous_overloading ~range))
          }
        |> Option.iter ~f:(add_handler_over over)
    ;;

    let iter t ~f =
      Map.iteri t.remaining_overs ~f:(fun ~key ~data:over ->
        f ~key ~data:over.over_scheme)
    ;;
  end

  let rec filter_over
            ~(state : State.t)
            ~(env : Env.t)
            ~(switch : Switch.t)
            ~with_
            ~closure
            ~over
            over_type_p
            expected_type_p
    =
    Switch.match_
      ~state
      ~env
      ~in_:switch
      ~curr_region:`Env
      ~over
      expected_type_p
      ~closure
      ~with_:(fun ~shape:expected_type_p_shape ~args:expected_type_p_args ->
        (* If the current type in the overloaded scheme is generic and a variable, 
           then don't register a match constraint on it *)
        if
          G.Type.is_generic over_type_p
          &&
          match G.Type.inner over_type_p with
          | Var -> true
          | _ -> false
        then ()
        else
          Switch.match_
            ~state
            ~env
            ~in_:switch
            ~curr_region:`Over
            ~over
            over_type_p
            ~closure:(closure @ expected_type_p_args)
            ~with_:(fun ~shape:over_type_p_shape ~args:over_type_p_args ->
              if Principal_shape.equal expected_type_p_shape over_type_p_shape
              then
                List.iter2_exn
                  expected_type_p_args
                  over_type_p_args
                  ~f:(fun expected_type_p_arg over_type_p_arg ->
                    filter_over
                      ~state
                      ~env
                      ~switch
                      ~closure
                      ~with_
                      ~over
                      over_type_p_arg
                      expected_type_p_arg)
              else (
                Switch.remove switch over;
                try Switch.try_resolve switch ~f:with_ with
                | Switch.Not_resolved -> ())))
  ;;

  let match_ ~state ~env expected_type ~in_:overloads ~closure ~with_ =
    let switch = Switch.create overloads in
    try Switch.try_resolve switch ~f:with_ with
    | Switch.Not_resolved ->
      Switch.iter switch ~f:(fun ~key:over ~data:over_scheme ->
        filter_over
          ~state
          ~env
          ~switch
          ~closure
          ~with_
          ~over
          over_scheme.root
          expected_type)
  ;;
end

let instantiate ~(state : State.t) ~(env : Env.t) gscheme expected_type =
  let instantiation, implicits, actual_type =
    G.instantiate ~state ~curr_region:env.curr_region gscheme
  in
  unify ~state ~env actual_type expected_type;
  instantiation, implicits
;;

let rec resolve_implicit
          ~state
          ~env
          ~root_witness
          ~curr_witness
          ~implicit_env
          expected_type
  =
  Overload.match_
    ~state
    ~env
    expected_type
    ~in_:implicit_env
    ~closure:[ expected_type ]
    ~with_:(fun head gscheme ->
      (* Instantiate the type scheme*)
      let instantiation, implicits = instantiate ~state ~env gscheme expected_type in
      (* Allocate witnesses for each implicit and set the current witness *)
      let implicit_witnesses =
        List.map implicits ~f:(fun _implicit -> Termination.Witness.hole ())
      in
      (curr_witness
       := Termination.Witness.(
            Spine (Spine.create ~head ~instantiation ~args:implicit_witnesses)));
      (* Mark the variable in the history *)
      let env = Env.mark_var_in_termination_history env head in
      let env =
        match
          Termination.Check.check_if_exceeded_threshold
            env.termination_check
            env.termination_history
            ~on:head
            ~root_witness
        with
        | `No_check -> env
        | `Check rejects ->
          if rejects
          then Env.raise env Resolution_termination_check_failed
          else
            (* TODO: Reset globally instead of just in current path *)
            Env.reset_var_in_termination_history env head
      in
      (* Safety: List.length implicit_witnesses = List.length implicits by construction *)
      List.iter2_exn implicits implicit_witnesses ~f:(fun implicit implicit_witness ->
        resolve_implicit
          ~state
          ~env
          ~root_witness
          ~curr_witness:implicit_witness
          ~implicit_env
          implicit))
;;

let resolve_toplevel_implicits ~state ~env implicits =
  let implicit_env = Env.implicit_env env in
  List.iter implicits ~f:(fun implicit ->
    let root_witness = Termination.Witness.hole () in
    resolve_implicit
      ~state
      ~env
      ~root_witness
      ~curr_witness:root_witness
      ~implicit_env
      implicit)
;;

let explicit_instantiate ~state ~env gscheme expected_type =
  let _, implicits = instantiate ~state ~env gscheme expected_type in
  match implicits with
  | [] ->
    (* Optimize for common case *)
    ()
  | _ :: _ -> resolve_toplevel_implicits ~state ~env implicits
;;

let over_instantiate ~state ~env ~overs expected_type =
  Overload.match_
    ~state
    ~env
    expected_type
    ~in_:overs
    ~closure:[ expected_type ]
    ~with_:(fun _ gscheme -> explicit_instantiate ~state ~env gscheme expected_type)
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
    explicit_instantiate ~state ~env var_gscheme expected_gtype
  | Over_instance (vars, expected_type) ->
    [%log.global.debug "Decoding expected_type" (expected_type : Type.t)];
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    [%log.global.debug "Decoded expected_type" (expected_gtype : G.Type.t)];
    let overs = List.map vars ~f:(fun over_var -> over_var, Env.find_var env over_var) in
    over_instantiate ~state ~env ~overs expected_gtype
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
  | Let_implicit (var_name, in_) ->
    let env = Env.add_implicit_var env var_name in
    self ~state ~env in_
  | Implicit expected_type ->
    [%log.global.debug "Decoding expected_type" (expected_type : Type.t)];
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    [%log.global.debug "Decoded expected_type" (expected_gtype : G.Type.t)];
    resolve_toplevel_implicits ~state ~env [ expected_gtype ]
  | With_range (t, range) -> solve ~state ~env:(Env.with_range env ~range) t

and solve_let_binding ~state ~env { type_vars; implicits; in_; bindings } =
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
  let gimplicits =
    List.map implicits ~f:(fun implicit -> gtype_of_type ~state ~env implicit)
  in
  let gbindings =
    List.map bindings ~f:(fun { binding_var; binding_type } ->
      [%log.global.debug
        "Decoding binding type" (binding_var : Constraint.Var.t) (binding_type : Type.t)];
      let binding_gtype = gtype_of_type ~state ~env binding_type in
      [%log.global.debug
        "Type of binding" (binding_var : Constraint.Var.t) (binding_gtype : G.Type.t)];
      let gscheme = Env.create_scheme env ~implicits:gimplicits binding_gtype in
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
  -> ?termination_check:Termination.Check.t
  -> Constraint.t
  -> (unit, Error.t) result
  =
  fun ?range ?defaulting ?(termination_check = Termination.Check.disabled) cst ->
  try
    Scheduler.(clear (t ()));
    let state = State.create ?defaulting () in
    let root_region = State.root_region state in
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
