open! Import
module F = Structure.Former
module M = Structure.Shape_var (F)
module R = Structure.Rigid (M)
module I = Structure.First_order (R)

module Pool = struct
  type 'node t =
    { mutable rigid_vars : 'node list
    ; mutable shape_var_region : Principal_shape.Var.Region.t option
    ; parent_shape_var_region : Principal_shape.Var.Region.t
    ; raise_scope_escape : ('node -> unit[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ~raise_scope_escape ~parent_shape_var_region () =
    { rigid_vars = []
    ; shape_var_region = None
    ; parent_shape_var_region
    ; raise_scope_escape
    }
  ;;

  let register_rigid_var t rigid_var = t.rigid_vars <- rigid_var :: t.rigid_vars
end

module Instance_identifier = Identifier

module S = struct
  type 'node t =
    { inner : 'node I.t
    ; instances : 'node instances
    }

  and 'node instances = (int * 'node) Instance_identifier.Map.t [@@deriving sexp_of]

  type 'node ctx =
    { remove_guard : 'node -> unit
    ; prune_structure : 'node M.t -> 'node instances -> unit
    ; prune_instances : 'node -> unit
    ; shape_var_state : Principal_shape.Var.State.t
    ; scheduler : Scheduler.t
    }

  let decomposition_of_structure = fun { F.args; shape } -> args, shape

  let super_ctx ctx =
    { M.super = ()
    ; shape_var_state = ctx.shape_var_state
    ; decomposition_of_structure
    ; scheduler = ctx.scheduler
    }
  ;;

  exception Cannot_merge = I.Cannot_merge

  let iter t ~f = I.iter t.inner ~f
  let is_var t = I.is_var t.inner

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    let create inner = create { inner; instances = Instance_identifier.Map.empty } in
    let inner =
      I.merge ~ctx:(super_ctx ctx) ~create ~unify ~type1 ~type2 t1.inner t2.inner
    in
    (match inner with
     | Structure (Structure structure) ->
       (* Notify instances of new structure *)
       if is_var t1 then ctx.prune_structure structure t1.instances;
       if is_var t2 then ctx.prune_structure structure t2.instances
     | _ -> ());
    let instances =
      Map.merge_skewed
        t1.instances
        t2.instances
        ~combine:(fun ~key:_ (level1, inst1) (level2, inst2) ->
          assert (level1 = level2);
          (* Both edges belong to the same instance group. The second edge is
             redundant after their destinations have been unified. *)
          ctx.remove_guard inst2;
          unify inst1 inst2;
          level1, inst1)
    in
    (* The generalizer may lower the instances. Once unification completes,
       we should prune instances. We only need to prune the instances of
       one of the types (since they're merged after unification) *)
    ctx.prune_instances type1;
    { inner; instances }
  ;;

  module Region_metadata = struct
    type 'node t = 'node Pool.t [@@deriving sexp_of]
  end

  module Propagation = struct
    type ctx =
      { shape_var_state : Principal_shape.Var.State.t
      ; scheduler : Scheduler.t
      }

    type 'node target = Principal_shape.Var.t

    let iter_targets t ~f =
      match t.inner with
      | Structure (Structure (Shape_app { shape_var; args = _ })) -> f shape_var
      | Var | Structure Rigid_var | Structure (Structure (Shape_args _ | Structure _)) ->
        ()
    ;;

    let root_derived ~ctx ~source:_ target ~by =
      Principal_shape.Var.add_guard ~state:ctx.shape_var_state target by
    ;;

    let clear_derived ~ctx ~source:_ target ~by =
      Principal_shape.Var.clear_guard ~state:ctx.shape_var_state target by
    ;;
  end
end

module G = Omniml_collector.Make (S)

module State = struct
  type t =
    { id_source : (Identifier.source[@sexp.opaque])
    ; type_state : G.State.t
    ; shape_var_state : Principal_shape.Var.State.t
    ; scheduler : Scheduler.t
    ; defaulting : Omniml_options.Defaulting.t
    }
  [@@deriving sexp_of]

  let create ?(defaulting = Omniml_options.Defaulting.default) () =
    let id_source = Identifier.create_source () in
    let scheduler = Scheduler.create () in
    let shape_var_state = Principal_shape.Var.State.create ~id_source in
    let root_shape_var_region = Principal_shape.Var.Region.root ~state:shape_var_state in
    let root_pool =
      Pool.create
        ~raise_scope_escape:(fun _ ->
          raise_bug_s
            ~here:[%here]
            [%message "The root region should not bind any rigid variables"])
        ~parent_shape_var_region:root_shape_var_region
        ()
    in
    let type_state = G.State.create ~id_source ~root:root_pool in
    { id_source; type_state; shape_var_state; scheduler; defaulting }
  ;;

  let root_region t = G.State.root_region t.type_state
  let root_shape_var_region t = Principal_shape.Var.Region.root ~state:t.shape_var_state
  let num_alive_regions t = G.State.num_alive_regions t.type_state
  let is_quiescent t = G.State.is_quiescent t.type_state
end

module Region = struct
  type t = G.Region.t [@@deriving sexp_of]

  let pool = G.Region.metadata
  let parent = G.Region.parent
  let level t = (G.Region.level t :> int)
  let register_rigid_var t rigid_var = Pool.register_rigid_var (pool t) rigid_var
end

module Type = struct
  type t = G.Node.t [@@deriving sexp_of]

  let structure = G.Node.structure
  let is_representative = G.Node.is_representative
  let same_class = G.Node.same_class
  let id = G.Node.id
  let inner t = (structure t).S.inner
  let region = G.Node.region
  let level t = Region.level (region t)
  let is_generic = G.Node.is_dead
  let is_var t = S.is_var (structure t)
  let instances t = (structure t).S.instances

  module Mark = struct
    type 'a t = (Identifier.t, 'a) Hashtbl.t [@@deriving sexp_of]

    let create () = Hashtbl.create (module Identifier)

    let mark t term data =
      match Hashtbl.add t ~key:(id term) ~data with
      | `Ok -> true
      | `Duplicate -> false
    ;;
  end

  let try_mark t mark data = Mark.mark mark t data

  let update_structure ~state t f =
    G.Node.update_structure ~state:state.State.type_state t ~f
  ;;

  let add_instance ~state t iid instance =
    update_structure ~state t (fun structure ->
      { structure with
        instances = Map.add_exn structure.instances ~key:iid ~data:instance
      })
  ;;

  let add_guard ~state t = G.Node.Rooting.root ~state:state.State.type_state t
  let remove_guard ~state t = G.Node.Rooting.unroot ~state:state.State.type_state t

  let create ~(state : State.t) ~curr_region inner =
    G.Node.create
      ~state:state.type_state
      ~curr_region
      { S.inner; instances = Instance_identifier.Map.empty }
  ;;

  module Unsafe = struct
    include G.Node.Unsafe

    let lower = promote
  end
end

module Unify = struct
  exception Unify = G.Node.Unify
end

module Scheme = struct
  type t =
    { root : Type.t
    ; region : Region.t
    }
  [@@deriving sexp_of]

  let body t = t.root

  let iter_instances_and_partial_generics t ~f =
    let visited = Hash_set.create (module Identifier) in
    let rec loop type_ =
      let id = Type.id type_ in
      if not (Hash_set.mem visited id)
      then (
        Hash_set.add visited id;
        if Type.is_generic type_ then I.iter (Type.inner type_) ~f:loop else f type_)
    in
    loop t.root
  ;;
end

let create_var ~state ~curr_region () = Type.create ~state ~curr_region Var

let create_shape_var ~(state : State.t) ~curr_region ?shape ?defaulted () =
  Principal_shape.Var.create
    ~state:state.shape_var_state
    ~region:(Region.pool curr_region).parent_shape_var_region
    ?shape
    ?defaulted
    ()
;;

let create_rigid_var ~state ~curr_region () =
  let rigid_var = Type.create ~state ~curr_region (Structure Rigid_var) in
  Region.register_rigid_var curr_region rigid_var;
  rigid_var
;;

let create_shape_args ~state ~curr_region args =
  Type.create ~state ~curr_region (Structure (Structure (Shape_args args)))
;;

let create_shape_app ~state ~curr_region args shape_var =
  Type.create ~state ~curr_region (Structure (Structure (Shape_app { args; shape_var })))
;;

let create_former ~state ~curr_region former =
  Type.create ~state ~curr_region (Structure (Structure (Structure former)))
;;

let flexize_inner = function
  | I.Structure Rigid_var -> I.Var
  | inner -> inner
;;

let prune_instances ~(state : State.t) type_ ~unify_inst =
  let dst = Type.region type_ in
  let structure = Type.structure type_ in
  let instances =
    Map.filter structure.instances ~f:(fun (src_level, instance) ->
      if src_level > Region.level dst
      then (
        Type.remove_guard ~state instance;
        unify_inst type_ instance;
        false)
      else true)
  in
  S.Propagation.iter_targets structure ~f:(fun shape_var ->
    Principal_shape.Var.unsafe_lower
      ~state:state.shape_var_state
      shape_var
      ~into:(Region.pool dst).parent_shape_var_region);
  (* Safety: prune_instances is called after unification (immediately after
     the dirty bit has been set) or by the generalizer.  *)
  Type.Unsafe.set_structure type_ { structure with instances }
;;

let rec prune_structure ~(state : State.t) structure instances =
  Map.iteri instances ~f:(fun ~key:instance_id ~data:(source_level, instance) ->
    let dst_region = Type.region instance in
    let copy =
      Type.create
        ~state
        ~curr_region:dst_region
        (Structure
           (Structure
              (structure |> M.map ~f:(copy ~state ~instance_id ~source_level ~dst_region))))
    in
    unify ~state ~curr_region:dst_region copy instance)

and copy ~(state : State.t) type_ ~instance_id ~source_level ~dst_region =
  let generic_copies = Hashtbl.create (module Identifier) in
  let shape_var_copies = Hashtbl.create (module Identifier) in
  let rec visit type_ =
    if Type.level type_ < source_level
    then type_
    else if Type.is_generic type_
    then find_or_alloc_generic_copy type_
    else find_or_alloc_instance_copy type_
  and copy_shape_var shape_var =
    if not (Principal_shape.Var.is_generic shape_var)
    then shape_var
    else
      Hashtbl.find_or_add
        shape_var_copies
        (Principal_shape.Var.id shape_var)
        ~default:(fun () ->
          let shape =
            if Principal_shape.Var.is_empty shape_var
            then None
            else Some (Principal_shape.Var.shape_exn shape_var)
          in
          create_shape_var
            ?shape
            ~defaulted:(Principal_shape.Var.defaulted shape_var)
            ~state
            ~curr_region:dst_region
            ())
  and copy_inner = function
    | I.Structure (R.Structure (M.Shape_app { args; shape_var })) ->
      I.Structure
        (R.Structure
           (M.Shape_app { args = visit args; shape_var = copy_shape_var shape_var }))
    | inner -> I.map inner ~f:visit
  and alloc_copy ~on_alloc type_ =
    let copy = create_var ~state ~curr_region:dst_region () in
    on_alloc copy;
    let inner = Type.inner type_ in
    let inner = if Type.is_generic type_ then inner else flexize_inner inner in
    let inner_copy = copy_inner inner in
    unify
      ~state
      ~curr_region:dst_region
      copy
      (Type.create ~state ~curr_region:dst_region inner_copy);
    copy
  and find_or_alloc_generic_copy type_ =
    let id = Type.id type_ in
    Hashtbl.find_or_add generic_copies id ~default:(fun () ->
      alloc_copy
        ~on_alloc:(fun copy -> Hashtbl.set generic_copies ~key:id ~data:copy)
        type_)
  and find_or_alloc_instance_copy type_ =
    match Map.find (Type.instances type_) instance_id with
    | Some (_src_level, instance) -> instance
    | None ->
      alloc_copy
        ~on_alloc:(fun instance ->
          Type.add_guard ~state instance;
          Type.add_instance ~state type_ instance_id (source_level, instance))
        type_
  in
  visit type_

and unify ~(state : State.t) ~curr_region type1 type2 =
  let prune_instances_worklist = Queue.create () in
  let prune_structure_worklist = Queue.create () in
  let unify_inst_worklist = Queue.create () in
  let ctx : _ S.ctx =
    { remove_guard = Type.remove_guard ~state
    ; shape_var_state = state.shape_var_state
    ; scheduler = state.scheduler
    ; prune_structure =
        (fun structure instances ->
          Queue.enqueue prune_structure_worklist (structure, instances))
    ; prune_instances =
        (fun type_ -> Queue.enqueue prune_instances_worklist (type_, Type.level type_))
    }
  in
  let unify type1 type2 =
    G.Node.try_unify_or_rollback ~state:state.type_state ~curr_region ~ctx type1 type2
  in
  unify type1 type2;
  let unify_inst type_ instance = Queue.enqueue unify_inst_worklist (type_, instance) in
  Queue.iter prune_instances_worklist ~f:(fun (type_, prev_level) ->
    (* Safety: LCA ensures that comparison by levels is safe here. *)
    if Type.level type_ < prev_level then prune_instances ~state type_ ~unify_inst);
  Queue.iter unify_inst_worklist ~f:(fun (type_, instance) -> unify type_ instance);
  Queue.iter prune_structure_worklist ~f:(fun (structure, instances) ->
    prune_structure ~state structure instances)
;;

let new_region ~state ~raise_scope_escape curr_region =
  let shape_var_region =
    match state.State.defaulting with
    | Disabled -> State.root_shape_var_region state
    | Unary ->
      let pool = Region.pool curr_region in
      (match pool.shape_var_region with
       | Some region -> region
       | None ->
         let region =
           Principal_shape.Var.Region.create
             ~state:state.shape_var_state
             ~parent:pool.parent_shape_var_region
         in
         pool.shape_var_region <- Some region;
         region)
  in
  let pool =
    Pool.create ~raise_scope_escape ~parent_shape_var_region:shape_var_region ()
  in
  G.Region.create ~state:state.type_state ~parent:curr_region pool
;;

let create_scheme ~curr_region root : Scheme.t = { root; region = curr_region }
let run_scheduler state () = Scheduler.run state.State.scheduler

let propagation_ctx (state : State.t) : S.Propagation.ctx =
  { shape_var_state = state.shape_var_state; scheduler = state.scheduler }
;;

let rigid_scope_check region =
  let pool = Region.pool region in
  match
    List.find pool.rigid_vars ~f:(fun var -> Type.level var < Region.level region)
  with
  | None -> ()
  | Some var -> pool.raise_scope_escape var
;;

let finalize ~state type_ =
  let structure = Type.structure type_ in
  Map.iter structure.instances ~f:(fun (_source_level, instance) ->
    Type.remove_guard ~state instance);
  (* Safety: the generalizer owns [type_] while running [finalize]. *)
  G.Node.Unsafe.set_structure
    type_
    { inner = flexize_inner structure.inner; instances = Instance_identifier.Map.empty }
;;

let promote ~state type_ =
  prune_instances ~state type_ ~unify_inst:(fun type_ instance ->
    Scheduler.enqueue state.State.scheduler (fun () ->
      unify ~state ~curr_region:(Type.region type_) type_ instance))
;;

let force_generalization ~(state : State.t) region =
  let rec loop () =
    G.collect_region
      ~state:state.State.type_state
      ~ctx:(propagation_ctx state)
      ~before_mark:(run_scheduler state)
      ~before_sweep:rigid_scope_check
      ~promote:(promote ~state)
      ~finalize:(finalize ~state)
      ~after_sweep:(run_scheduler state)
      region;
    (match state.defaulting with
     | Disabled -> ()
     | Unary ->
       Option.iter
         (Region.pool region).shape_var_region
         ~f:
           (Principal_shape.Var.generalize
              ~state:state.shape_var_state
              ~on_generalize:
                (Principal_shape.Var.default_on_generalize
                   ~state:state.shape_var_state
                   ~scheduler:state.scheduler)));
    if not (Scheduler.is_maintenance_empty state.scheduler)
    then (
      (* If forcing occurs inside a handler, newly queued handlers must remain
         deferred. Retrying for them here would violate non-reentrancy and
         could never make progress. *)
      run_scheduler state ();
      loop ())
  in
  loop ()
;;

let force_root_generalization_and_return_unsolved_shape_var_errors ~(state : State.t) =
  let generalize_roots () =
    G.collect_all_regions
      ~state:state.type_state
      ~ctx:(propagation_ctx state)
      ~before_mark:(run_scheduler state)
      ~before_sweep:rigid_scope_check
      ~promote:(promote ~state)
      ~finalize:(finalize ~state)
      ~after_sweep:(run_scheduler state)
      ()
  in
  let collected_errors =
    match state.defaulting with
    | Disabled ->
      generalize_roots ();
      let errors = ref [] in
      Principal_shape.Var.generalize_all
        ~state:state.shape_var_state
        ~on_generalize:(Principal_shape.Var.cancel_on_generalize ~errors)
        ();
      !errors
    | Unary ->
      let rec default_until_quiet () =
        generalize_roots ();
        Principal_shape.Var.generalize_all
          ~state:state.shape_var_state
          ~on_generalize:
            (Principal_shape.Var.default_on_generalize
               ~state:state.shape_var_state
               ~scheduler:state.scheduler)
          ();
        if not (Scheduler.is_empty state.scheduler)
        then (
          run_scheduler state ();
          default_until_quiet ())
        else if not (G.State.is_quiescent state.type_state)
        then default_until_quiet ()
      in
      default_until_quiet ();
      []
  in
  let remaining_errors = ref [] in
  Principal_shape.Var.State.shape_vars state.shape_var_state
  |> List.iter ~f:(Principal_shape.Var.cancel_on_generalize ~errors:remaining_errors);
  collected_errors @ !remaining_errors
;;

let instantiate ~state ~curr_region ({ root; region = src_region } : Scheme.t) =
  force_generalization ~state src_region;
  let instance_id = Instance_identifier.create state.State.id_source in
  copy
    ~state
    ~source_level:(Region.level src_region)
    ~dst_region:curr_region
    ~instance_id
    root
;;

module Suspended_match = struct
  module Error = struct
    type t =
      | Cannot_default
      | Matchee_is_rigid
      | Inconsistent_default of
          { actual : Principal_shape.t
          ; expected : Principal_shape.t
          }
    [@@deriving sexp]
  end

  type t =
    { matchee : Type.t
    ; closure : closure
    ; case : curr_region:Region.t -> shape:Principal_shape.t -> args:Type.t list -> unit
    ; else_ : unit -> Principal_shape.t
    ; error : Error.t -> Omniml_error.t
    }
  [@@deriving sexp_of]

  and closure =
    { variables : Type.t list
    ; schemes : Scheme.t list
    }
  [@@deriving sexp_of]

  let closure_add_guard ~state ~shape_args { variables; schemes } =
    Type.add_guard ~state shape_args;
    List.iter variables ~f:(Type.add_guard ~state);
    List.iter schemes ~f:(fun scheme ->
      (* TODO: We should propably force the generalization of the scheme here *)
      [%log.global.debug "Guarding scheme" (scheme : Scheme.t)];
      Scheme.iter_instances_and_partial_generics scheme ~f:(Type.add_guard ~state);
      [%log.global.debug "Guarded scheme" (scheme : Scheme.t)])
  ;;

  let closure_remove_guard ~state ~shape_args { variables; schemes } =
    Type.remove_guard ~state shape_args;
    List.iter variables ~f:(Type.remove_guard ~state);
    List.iter schemes ~f:(fun scheme ->
      Scheme.iter_instances_and_partial_generics scheme ~f:(Type.remove_guard ~state))
  ;;

  exception Cannot_match_on_rigid of Omniml_error.t
  exception Inconsistent_defaults of Omniml_error.t

  let match_or_yield ~state ~curr_region { matchee; case; closure; else_; error } =
    let get_or_alloc_matchee_args () =
      match Type.inner matchee with
      | Structure (Structure (Structure { args; _ })) -> args
      | Structure (Structure (Shape_app { args; shape_var })) ->
        (match Type.inner args with
         | Structure (Structure (Shape_args args)) -> args
         | Var ->
           [%log.global.debug "Allocating matchee args" (args : Type.t)];
           let shape = Principal_shape.Var.shape_exn shape_var in
           let arg_types =
             List.init (Principal_shape.arity shape) ~f:(fun _ ->
               create_var ~state ~curr_region ())
           in
           let args' = create_shape_args ~state ~curr_region arg_types in
           [%log.global.debug
             "Allocated matchee args" (arg_types : Type.t list) (args' : Type.t)];
           [%log.global.debug
             "Unify (get_or_alloc_matchee_args)" (args : Type.t) (args' : Type.t)];
           unify ~state ~curr_region args args';
           [%log.global.debug "Unified matchee args" (args : Type.t)];
           arg_types
         | _ ->
           raise_bug_s
             ~here:[%here]
             [%message
               "Kind mismatch when allocating args. Expected args, got type."
                 (args : Type.t)])
      | Structure (Structure (Shape_args _)) ->
        raise_bug_s
          ~here:[%here]
          [%message
            "Kind mismatch when allocating args. Expected type, got args."
              (matchee : Type.t)]
      | Structure Rigid_var | Var ->
        raise_bug_s
          ~here:[%here]
          [%message "Matchee type cannot have undetermined structure" (matchee : Type.t)]
    in
    let add_handler ~shape_args svar =
      [%log.global.debug "Adding handler" (svar : Principal_shape.Var.t)];
      let discharged = ref false in
      let default = lazy (else_ ()) in
      closure_add_guard ~state ~shape_args closure;
      let release_guards () = closure_remove_guard ~state ~shape_args closure in
      Principal_shape.Var.add_handler
        ~scheduler:state.scheduler
        svar
        { run =
            (fun shape ->
              if not !discharged
              then (
                discharged := true;
                let args = get_or_alloc_matchee_args () in
                (* Solve case *)
                case ~curr_region ~shape ~args;
                [%log.global.debug "Finished solving suspended match case"];
                release_guards ()))
        ; default =
            (fun () ->
              if not !discharged
              then (
                [%log.global.debug
                  "Default handler triggered" (svar : Principal_shape.Var.t)];
                let default_shape = Lazy.force default in
                [%log.global.debug "Default shape" (default_shape : Principal_shape.t)];
                try
                  Principal_shape.Var.fill_exn
                    svar
                    ~scheduler:state.scheduler
                    default_shape
                with
                | Principal_shape.Var.Not_empty ->
                  let actual = Principal_shape.Var.shape_exn svar in
                  let report =
                    error (Inconsistent_default { actual; expected = default_shape })
                  in
                  raise (Inconsistent_defaults report)))
        ; error = (fun () -> error Cannot_default)
        }
    in
    match Type.inner matchee with
    | Var ->
      let shape_var = create_shape_var ~state ~curr_region () in
      let shape_args = create_var ~state ~curr_region () in
      add_handler ~shape_args shape_var;
      unify
        ~state
        ~curr_region
        matchee
        (create_shape_app ~state ~curr_region shape_args shape_var)
    | Structure (Structure (Shape_app { args = shape_args; shape_var })) ->
      add_handler ~shape_args shape_var
    | Structure (Structure (Shape_args _)) ->
      raise_bug_s
        ~here:[%here]
        [%message
          "Kind mismatch when adding matchee handler. Expected type, got args."
            (matchee : Type.t)]
    | Structure Rigid_var -> raise (Cannot_match_on_rigid (error Matchee_is_rigid))
    | Structure (Structure (Structure { args; shape })) ->
      (* Optimisation: Immediately solve the case *)
      case ~curr_region ~shape ~args
  ;;
end
