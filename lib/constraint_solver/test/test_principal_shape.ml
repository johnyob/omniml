open Core
open Omniml_std
open Omniml_constraint_solver.For_testing

let%quick_test _ =
  fun (type_scheme :
        (Type.Scheme.t
        [@generator Quickcheckable.Type.Scheme.quickcheck_generator]
        [@shrinker Quickcheckable.Type.Scheme.quickcheck_shrinker])) ->
  let _, scheme_shape = Principal_shape.scheme_shape_decomposition type_scheme in
  Principal_shape.Scheme.invariant scheme_shape
;;

let%expect_test "conflicting soft fills revive a cancelled shape variable empty" =
  let id_source = Identifier.create_source () in
  let scheduler = Scheduler.create () in
  let shape1 = Principal_shape.tuple 2 in
  let shape2 = Principal_shape.tuple 3 in
  let events = ref [] in
  let handler name : Principal_shape.Var.Handler.t =
    { run =
        (fun shape ->
          let shape = if Principal_shape.equal shape shape1 then "shape1" else "shape2" in
          events := [%string "%{name}:%{shape}"] :: !events)
    ; cancel = (fun () -> events := [%string "%{name}:cancel"] :: !events)
    }
  in
  let shape_var = Principal_shape.Var.create ~id_source () in
  Principal_shape.Var.add_handler shape_var ~scheduler (handler "old");
  Principal_shape.Var.cancel_exn shape_var ~scheduler;
  Scheduler.run scheduler;
  Principal_shape.Var.fill_exn shape_var shape1 ~scheduler;
  (* Conflicting fills clear the soft shape evidence. *)
  Principal_shape.Var.fill_exn shape_var shape2 ~scheduler;
  let cancelled = shape_var in
  let shape_var = Principal_shape.Var.revive cancelled ~id_source in
  assert (Principal_shape.Var.is_cancelled cancelled);
  Principal_shape.Var.add_handler shape_var ~scheduler (handler "new");
  Principal_shape.Var.fill_exn shape_var shape2 ~scheduler;
  Scheduler.run scheduler;
  List.rev !events |> List.iter ~f:print_endline;
  [%expect
    {|
    old:cancel
    new:shape2
    |}]
;;

let%expect_test "an unfilled cancelled shape variable revives empty" =
  let id_source = Identifier.create_source () in
  let scheduler = Scheduler.create () in
  let shape = Principal_shape.tuple 2 in
  let ran = ref false in
  let shape_var = Principal_shape.Var.create ~id_source () in
  Principal_shape.Var.cancel_exn shape_var ~scheduler;
  let shape_var = Principal_shape.Var.revive shape_var ~id_source in
  Principal_shape.Var.add_handler
    shape_var
    ~scheduler
    { run = (fun actual -> ran := Principal_shape.equal actual shape)
    ; cancel = (fun () -> assert false)
    };
  Principal_shape.Var.fill_exn shape_var shape ~scheduler;
  Scheduler.run scheduler;
  print_s [%message (!ran : bool)];
  [%expect {| (!ran true) |}]
;;

let%expect_test "merging handlers revives a cancelled variable from soft evidence" =
  let id_source = Identifier.create_source () in
  let scheduler = Scheduler.create () in
  let shape = Principal_shape.tuple 2 in
  let events = ref [] in
  let cancelled = Principal_shape.Var.create ~id_source () in
  Principal_shape.Var.cancel_exn cancelled ~scheduler;
  Principal_shape.Var.fill_exn cancelled shape ~scheduler;
  let empty = Principal_shape.Var.create ~id_source () in
  Principal_shape.Var.add_handler
    empty
    ~scheduler
    { run =
        (fun actual ->
          assert (Principal_shape.equal actual shape);
          events := "old:run" :: !events)
    ; cancel = (fun () -> events := "old:cancel" :: !events)
    };
  Principal_shape.Var.unify ~scheduler cancelled empty;
  Scheduler.run scheduler;
  List.rev !events |> List.iter ~f:print_endline;
  [%expect {| old:run |}]
;;

let%expect_test "merging conflicting cancelled variables clears soft evidence" =
  let id_source = Identifier.create_source () in
  let scheduler = Scheduler.create () in
  let shape1 = Principal_shape.tuple 2 in
  let shape2 = Principal_shape.tuple 3 in
  let shape_var1 = Principal_shape.Var.create ~id_source () in
  let shape_var2 = Principal_shape.Var.create ~id_source () in
  Principal_shape.Var.cancel_exn shape_var1 ~scheduler;
  Principal_shape.Var.cancel_exn shape_var2 ~scheduler;
  Principal_shape.Var.fill_exn shape_var1 shape1 ~scheduler;
  Principal_shape.Var.fill_exn shape_var2 shape2 ~scheduler;
  Principal_shape.Var.unify ~scheduler shape_var1 shape_var2;
  let shape_var1 = Principal_shape.Var.revive shape_var1 ~id_source in
  let ran = ref false in
  Principal_shape.Var.add_handler
    shape_var1
    ~scheduler
    { run = (fun shape -> ran := Principal_shape.equal shape shape1)
    ; cancel = (fun () -> assert false)
    };
  Scheduler.run scheduler;
  print_s [%message "before fill" (!ran : bool)];
  Principal_shape.Var.fill_exn shape_var1 shape1 ~scheduler;
  Scheduler.run scheduler;
  print_s [%message "after fill" (!ran : bool)];
  [%expect
    {|
    ("before fill" (!ran false))
    ("after fill" (!ran true))
    |}]
;;
