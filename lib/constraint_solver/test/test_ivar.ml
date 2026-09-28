open Core
open Omniml_constraint_solver.For_testing

module Test = struct
  type t =
    { scheduler : Scheduler.t
    ; mutable events : Sexp.t list
    }

  let create () = { scheduler = Scheduler.create (); events = [] }

  let print t =
    (* Empty the scheduler *)
    Scheduler.run t.scheduler;
    (* Print and flush any pending events *)
    let pending = List.rev t.events in
    t.events <- [];
    List.iter pending ~f:print_s
  ;;

  let create_handler t ~on:ivar ?(name = "") ?(cancellable = false) () =
    let run = fun value -> t.events <- [%message "Run" name (value : int)] :: t.events in
    let cancel =
      Option.some_if cancellable (fun () ->
        t.events <- [%message "Cancel" name] :: t.events)
    in
    Ivar.upon ivar ~scheduler:t.scheduler ~run ?cancel ()
  ;;

  let cancel_handler t handler =
    print_s
      [%sexp
        (Ivar.Handler.cancel handler ~scheduler:t.scheduler
         : [ `Ok | `Already_scheduled | `Already_cancelled | `Cannot_cancel ])]
  ;;

  let fill t ~ivar ~with_:x =
    print_s [%sexp (Ivar.fill ivar x ~scheduler:t.scheduler : [ `Ok | `Full of int ])]
  ;;

  let cancel_all t ~on:ivar = Ivar.cancel_all ivar ~scheduler:t.scheduler
  let merge_ivar t ivar1 ivar2 ~f = Ivar.merge ivar1 ivar2 ~scheduler:t.scheduler ~f
end

let%expect_test "fill schedules pending handlers" =
  let t = Test.create () in
  let ivar = Ivar.create () in
  ignore (Test.create_handler t ~on:ivar ());
  Test.print t;
  [%expect {||}];
  Test.fill t ~ivar ~with_:1;
  [%expect {| Ok |}];
  Test.print t;
  [%expect {| (Run "" (value 1)) |}];
  Test.fill t ~ivar ~with_:2;
  [%expect {| (Full 1) |}];
  Test.print t;
  [%expect {| |}]
;;

let%expect_test "upon on a full ivar is already scheduled" =
  let t = Test.create () in
  let ivar = Ivar.create () in
  Test.fill t ~ivar ~with_:3;
  [%expect {| Ok |}];
  let handler = Test.create_handler t ~on:ivar ~cancellable:true () in
  Test.cancel_handler t handler;
  [%expect {| Already_scheduled |}];
  Test.print t;
  [%expect {| (Run "" (value 3)) |}]
;;

let%expect_test "an individual cancellable handler can be cancelled" =
  let t = Test.create () in
  let ivar = Ivar.create () in
  let handler = Test.create_handler t ~on:ivar ~cancellable:true () in
  Test.cancel_handler t handler;
  [%expect {| Ok |}];
  Test.cancel_handler t handler;
  [%expect {| Already_cancelled |}];
  Test.fill t ~ivar ~with_:1;
  [%expect {| Ok |}];
  Test.print t;
  [%expect {| (Cancel "") |}]
;;

let%expect_test "cancel_all retains uncancellable handlers" =
  let t = Test.create () in
  let ivar = Ivar.create () in
  ignore
    (Test.create_handler t ~on:ivar ~cancellable:true ~name:"one" () : _ Ivar.Handler.t);
  let uncancellable = Test.create_handler t ~on:ivar ~cancellable:false ~name:"two" () in
  Test.cancel_all t ~on:ivar;
  [%expect {||}];
  Test.cancel_handler t uncancellable;
  [%expect {| Cannot_cancel |}];
  Test.fill t ~ivar ~with_:1;
  [%expect {| Ok |}];
  Test.print t;
  [%expect
    {|
    (Cancel one)
    (Run two (value 1))
    |}]
;;

let%expect_test "merge combines cells and preserves pending handlers" =
  let t = Test.create () in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  ignore (Test.create_handler t ~on:ivar1 ~name:"one" () : _ Ivar.Handler.t);
  ignore (Test.create_handler t ~on:ivar2 ~name:"two" () : _ Ivar.Handler.t);
  Test.merge_ivar t ivar1 ivar2 ~f:( + );
  Test.fill t ~ivar:ivar2 ~with_:7;
  [%expect {| Ok |}];
  Test.print t;
  [%expect
    {|
    (Run one (value 7))
    (Run two (value 7))
    |}];
  Test.fill t ~ivar:ivar1 ~with_:9;
  [%expect {| (Full 7) |}]
;;

let%expect_test "merge combines two full values" =
  let t = Test.create () in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  Test.fill t ~ivar:ivar1 ~with_:2;
  [%expect {| Ok |}];
  Test.fill t ~ivar:ivar2 ~with_:3;
  [%expect {| Ok |}];
  Test.merge_ivar t ivar1 ivar2 ~f:( + );
  Test.fill t ~ivar:ivar1 ~with_:0;
  [%expect {| (Full 5) |}];
  Test.fill t ~ivar:ivar2 ~with_:0;
  [%expect {| (Full 5) |}]
;;
