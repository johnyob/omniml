open! Import

let default () = Log.create ~output:stderr ()
let current = ref (default ())
let trace_file = ref None
let get () = !current
let set log = current := log
let reset () = current := default ()
let level () = Log.level !current
let set_level level = Log.set_level !current level
let set_output output = Log.set_output !current output
let would_log level = Log.would_log !current level
let log () = !current
let clear () = Log.clear !current
let close () = Log.close !current
let set_trace_file path = trace_file := path

let flush () =
  Option.iter !trace_file ~f:(fun path ->
    let log = log () in
    Log.close log;
    Chrome_tracing.write_file path log)
;;

let () = Stdlib.at_exit flush

let set_level_via_param () =
  let open Command.Param in
  map
    (flag "log-level" (optional Level.arg_type) ~doc:"LEVEL Set the log level")
    ~f:(Option.iter ~f:set_level)
;;

let set_trace_file_via_param () =
  let open Command.Param in
  map
    (flag
       "trace-file"
       (optional string)
       ~doc:"FILE Write Chrome tracing JSON (use -log-level debug for solver events)")
    ~f:set_trace_file
;;

module Span = struct
  let with_ ?level ?source ?fields name ~f =
    Log.Span.with_ !current ?level ?source ?fields name ~f
  ;;
end

module Event = struct
  let emit ?level ?source ?fields name =
    Log.Event.emit !current ?level ?source ?fields name
  ;;
end

module Track = struct
  let set track = Log.Track.set !current track
  let with_ track ~f = Log.Track.with_ !current track ~f
end

module For_testing = struct
  let use_test_output () = set_output (Some stdout)
end
