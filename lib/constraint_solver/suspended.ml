open! Import
module G = Generalization

module Closure = struct
  type t =
    { types : G.Type.t list
    ; schemes : G.Scheme.t list
    }
  [@@deriving sexp_of]

  let iter_types { types; schemes } ~f =
    List.iter types ~f;
    List.iter schemes ~f:(fun scheme ->
      G.Scheme.iter_instances_and_partial_generics scheme ~f)
  ;;

  let add_guards t ~gstate = iter_types t ~f:(G.Type.add_guard ~state:gstate)
  let remove_guards t ~gstate = iter_types t ~f:(G.Type.remove_guard ~state:gstate)
end

type t =
  { id : Identifier.t
  ; shape_var : Principal_shape.Var.t
  ; closure : Closure.t
  }
[@@deriving sexp_of]

and suspended = t

module State = struct
  type t =
    { id_source : (Identifier.source[@sexp.opaque])
    ; suspensions : (Identifier.t, suspended) Hashtbl.t
    }
  [@@deriving sexp_of]

  let create () =
    { id_source = Identifier.create_source ()
    ; suspensions = Hashtbl.create (module Identifier)
    }
  ;;
end

let create ~(state : State.t) ~gstate ~shape_var ~closure =
  let id = Identifier.create state.id_source in
  let suspended = { id; shape_var; closure } in
  Closure.add_guards closure ~gstate;
  Hashtbl.set state.suspensions ~key:id ~data:suspended;
  suspended
;;

let mark_resumed t ~(state : State.t) = Hashtbl.remove state.suspensions t.id
let free t ~gstate = Closure.remove_guards t.closure ~gstate

module Defaulting = struct
  let loop_generalization_until_stable ~gstate ~scheduler ~f =
    let rec loop () =
      G.generalize_all_regions ~state:gstate ~scheduler ();
      assert (Scheduler.is_empty scheduler);
      f ();
      if not (Scheduler.is_empty scheduler) then loop ()
    in
    loop ()
  ;;

  let cancel_all ~(state : State.t) ~gstate ~scheduler =
    loop_generalization_until_stable ~gstate ~scheduler ~f:(fun () ->
      let suspensions = Hashtbl.data state.suspensions in
      Hashtbl.clear state.suspensions;
      List.iter suspensions ~f:(fun t ->
        (* Note: repeated cancellations is a no-op *)
        Principal_shape.Var.cancel_exn t.shape_var ~scheduler))
  ;;

  module Shape_var_dependencies = struct
    type t =
      { shape_var : Principal_shape.Var.t
      ; shape_var_dep_ids : Identifier.t Hash_set.t
      }

    let create shape_var =
      { shape_var; shape_var_dep_ids = Hash_set.create (module Identifier) }
    ;;

    let is_defaultable { shape_var; shape_var_dep_ids; _ } =
      Hash_set.is_empty shape_var_dep_ids
      || (Hash_set.length shape_var_dep_ids = 1
          && Hash_set.mem shape_var_dep_ids (Principal_shape.Var.id shape_var))
    ;;

    let add_shape_var_dep t ~dep =
      Hash_set.add t.shape_var_dep_ids (Principal_shape.Var.id dep)
    ;;
  end

  let unary ~(state : State.t) ~gstate ~scheduler =
    let iter_unary_shape_vars ~f =
      let shape_var_deps = Hashtbl.create (module Identifier) in
      let find_or_alloc_shape_var_deps shape_var =
        Hashtbl.find_or_add
          shape_var_deps
          (Principal_shape.Var.id shape_var)
          ~default:(fun () -> Shape_var_dependencies.create shape_var)
      in
      let visit_suspended suspended =
        let mark = G.Type.Mark.create () in
        let rec visit_type type_ =
          if G.Type.try_mark type_ mark ()
          then (
            (match G.Type.inner type_ with
             | Structure (Shape_var shape_var) ->
               shape_var
               |> find_or_alloc_shape_var_deps
               |> Shape_var_dependencies.add_shape_var_dep ~dep:suspended.shape_var
             | _ -> ());
            G.(Type.inner type_ |> I.iter ~f:visit_type);
            Map.iter (G.Type.instances type_) ~f:(fun (_src_level, instance) ->
              visit_type instance))
        in
        Closure.iter_types suspended.closure ~f:visit_type
      in
      Hashtbl.iter state.suspensions ~f:visit_suspended;
      Hashtbl.data shape_var_deps
      |> List.filter ~f:Shape_var_dependencies.is_defaultable
      |> List.iter ~f:(fun deps -> f deps.shape_var)
    in
    loop_generalization_until_stable ~gstate ~scheduler ~f:(fun () ->
      iter_unary_shape_vars ~f:(fun shape_var ->
        Principal_shape.Var.cancel_exn shape_var ~scheduler))
  ;;
end

let get_or_alloc_shape_args ~gstate ~scheduler ~curr_region ~shape type_ =
  let args =
    List.init (Principal_shape.arity shape) ~f:(fun _ ->
      G.create_var ~state:gstate ~curr_region ())
  in
  G.unify
    ~state:gstate
    ~scheduler
    ~curr_region
    type_
    (G.create_former ~state:gstate ~curr_region { shape; args });
  args
;;

let match_
      ~(state : State.t)
      ~gstate
      ~scheduler
      ~curr_region
      matchee
      ~with_
      ~closure
      ~default
  =
  (* Add [matchee] to closure since [get_matchee_args] relies on it being live. *)
  let closure = Closure.{ closure with types = matchee :: closure.types } in
  let add_handler shape_var =
    let t = create ~state ~gstate ~closure ~shape_var in
    Principal_shape.Var.add_handler
      shape_var
      ~scheduler
      { run =
          (fun shape ->
            let args =
              get_or_alloc_shape_args ~gstate ~scheduler ~curr_region ~shape matchee
            in
            mark_resumed t ~state;
            with_ ~shape ~args;
            free t ~gstate)
      ; cancel =
          (fun () ->
            mark_resumed t ~state;
            default ();
            free t ~gstate)
      }
  in
  match G.Type.inner matchee with
  | Var ->
    let shape_var = Principal_shape.Var.create ~id_source:gstate.id_source () in
    add_handler shape_var;
    G.unify_var
      ~state:gstate
      ~curr_region
      matchee
      (G.Type.create ~state:gstate ~curr_region (Structure (Shape_var shape_var)))
  | Structure (Shape_var shape_var) -> add_handler shape_var
  | Structure (Structure Rigid_var) -> default ()
  | Structure (Structure (Structure { args; shape })) ->
    (* Optimisation: Immediately solve the case *)
    with_ ~shape ~args
;;
