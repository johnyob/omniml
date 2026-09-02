open! Core
open! Omniml_log

let show_raise f =
  match f () with
  | () -> print_endline "did not raise"
  | exception exn -> print_s [%sexp (exn : Exn.t)]
;;

let print_structure log =
  List.iter (Log.to_list log) ~f:(fun event ->
    let sequence = Event.sequence event in
    let track = Event.track event in
    match Event.desc event with
    | Event.Span_begin { id; parent; name; fields } ->
      print_s
        [%message
          "begin"
            (sequence : int)
            (track : int)
            (id : int)
            (parent : int option)
            name
            (fields : Event.field list)]
    | Span_end { id; name } ->
      print_s [%message "end" (sequence : int) (track : int) (id : int) name]
    | Message { level; message; fields } ->
      print_s
        [%message
          "message"
            (sequence : int)
            (track : int)
            (level : Level.t)
            (message : Sexp_or_string.t)
            (fields : Event.field list)])
;;

let%expect_test "nested structured trace" =
  let log = Log.create ~level:`Trace () in
  Log.Span.with_
    log
    ~level:`Debug
    ~fields:(fun () -> [ "constraint", [%sexp "int = bool"] ])
    "solve"
    ~f:(fun () ->
      Log.Event.emit log ~level:`Info "checkpoint";
      Log.Span.with_ log "unify" ~f:(fun () -> ()));
  Log.close log;
  print_structure log;
  [%expect
    {|
    (begin (sequence 0) (track 0) (id 0) (parent ()) solve
     (fields ((constraint "int = bool"))))
    (message (sequence 1) (track 0) (level Info) (message (String checkpoint))
     (fields ()))
    (begin (sequence 2) (track 0) (id 1) (parent (0)) unify (fields ()))
    (end (sequence 3) (track 0) (id 1) unify)
    (end (sequence 4) (track 0) (id 0) solve)
    |}]
;;

let%expect_test "disabled fields are lazy" =
  let log = Log.create ~level:`Info () in
  let evaluated = ref false in
  Log.Event.emit
    log
    ~level:`Debug
    ~fields:(fun () ->
      evaluated := true;
      [])
    "disabled";
  Log.close log;
  print_s [%message (!evaluated : bool) (log : Log.t)];
  [%expect {| ((!evaluated false) (log ())) |}]
;;

let%expect_test "closed logs are immutable" =
  let log = Log.create () in
  Log.close log;
  Log.Event.emit log "ignored";
  print_s [%message (Log.is_closed log : bool) (Log.length log : int)];
  show_raise (fun () -> Log.clear log);
  [%expect
    {|
    (("Log.is_closed log" true) ("Log.length log" 0))
    (Invalid_argument "Omniml_log.Log.clear: the log is closed")
    |}]
;;

let%expect_test "Chrome export requires a closed log" =
  let log = Log.create () in
  show_raise (fun () -> Chrome_tracing.to_string log |> ignore);
  Log.close log;
  print_string (Chrome_tracing.to_string log);
  [%expect
    {|
    (Invalid_argument "Omniml_log.Log.to_list: the log is still open")
    {"displayTimeUnit":"ms","traceEvents":[]}
    |}]
;;

let%expect_test "ppx adapter retains sexp fields" =
  let log = Log.create ~level:`Debug () in
  Global.set log;
  [%log.global.debug "unify" (3 : int) ~ok:(true : bool)];
  Log.close log;
  print_structure log;
  [%expect
    {|
    (message (sequence 0) (track 0) (level Debug) (message (String unify))
     (fields ((3 3) (ok true))))
    |}]
;;

let%expect_test "Chrome trace JSON escapes values" =
  let log = Log.create ~level:`Debug () in
  Log.Event.emit
    log
    ~level:`Debug
    ~fields:(fun () -> [ "value", [%sexp "a\"b\n"] ])
    "event";
  Log.close log;
  print_string (Chrome_tracing.to_string log);
  [%expect
    {| {"displayTimeUnit":"ms","traceEvents":[{"name":"event","cat":"log","ph":"i","s":"t","ts":0,"pid":0,"tid":0,"args":{"level":"debug","value":"\"a\\\"b\\n\""}}]} |}]
;;
