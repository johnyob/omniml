open! Import
module Recorded_event = Event

type open_span = { id : int }

type t =
  { mutable events_rev : Recorded_event.t list
  ; mutable sequence : int
  ; mutable next_span_id : int
  ; mutable current_track : int
  ; stacks : open_span list Int.Table.t
  ; mutable level : Level.t
  ; mutable output : Out_channel.t option
  ; mutable closed : bool
  }

let create ?(level = `Info) ?output () =
  { events_rev = []
  ; sequence = 0
  ; next_span_id = 0
  ; current_track = 0
  ; stacks = Int.Table.create ()
  ; level
  ; output
  ; closed = false
  }
;;

let is_closed t = t.closed
let has_active_spans t = Hashtbl.data t.stacks |> List.exists ~f:(Fn.non List.is_empty)

let close t =
  if has_active_spans t then invalid_arg "Omniml_log.Log.close: a span is active";
  t.closed <- true
;;

let ensure_open t operation =
  if t.closed then invalid_argf "Omniml_log.Log.%s: the log is closed" operation ()
;;

let ensure_closed t operation =
  if not t.closed
  then invalid_argf "Omniml_log.Log.%s: the log is still open" operation ()
;;

let level t = t.level

let set_level t level =
  ensure_open t "set_level";
  t.level <- level
;;

let set_output t output =
  ensure_open t "set_output";
  t.output <- output
;;

let would_log t level = (not t.closed) && Level.compare_by_severity level t.level >= 0

let to_list t =
  ensure_closed t "to_list";
  List.rev t.events_rev
;;

let length t = List.length (to_list t)
let is_empty t = List.is_empty (to_list t)
let sexp_of_t t = [%sexp (to_list t : Recorded_event.t list)]

let clear t =
  ensure_open t "clear";
  if has_active_spans t then invalid_arg "Omniml_log.Log.clear: a span is active";
  t.events_rev <- [];
  t.sequence <- 0;
  t.next_span_id <- 0;
  Hashtbl.clear t.stacks
;;

let stack t = Hashtbl.find t.stacks t.current_track |> Option.value ~default:[]
let set_stack t stack = Hashtbl.set t.stacks ~key:t.current_track ~data:stack

let fields_as_text fields =
  List.map fields ~f:(fun (name, value) ->
    sprintf "%s=%s" name (Sexp.to_string_hum value))
  |> String.concat ~sep:" "
;;

let write_console t desc =
  Option.iter t.output ~f:(fun output ->
    let indentation = String.make (List.length (stack t) * 2) ' ' in
    let line =
      match desc with
      | Recorded_event.Span_begin { name; fields; _ } ->
        String.concat
          ~sep:" "
          (List.filter_opt
             [ Some (indentation ^ "SPAN " ^ name)
             ; Option.some_if (not (List.is_empty fields)) (fields_as_text fields)
             ])
      | Span_end { name; _ } -> indentation ^ "END " ^ name
      | Message { level; message; fields } ->
        String.concat
          ~sep:" "
          (List.filter_opt
             [ Some
                 (indentation
                  ^ String.uppercase (Level.to_string level)
                  ^ " "
                  ^ Sexp_or_string.to_string message)
             ; Option.some_if (not (List.is_empty fields)) (fields_as_text fields)
             ])
    in
    Out_channel.output_string output line;
    Out_channel.newline output;
    Out_channel.flush output)
;;

let record t event =
  ensure_open t "record";
  t.sequence <- t.sequence + 1;
  t.events_rev <- event :: t.events_rev;
  write_console t (Recorded_event.desc event)
;;

module Span = struct
  let with_ t ?(level = `Debug) ?source ?(fields = fun () -> []) name ~f =
    if not (would_log t level)
    then f ()
    else (
      let id = t.next_span_id in
      t.next_span_id <- id + 1;
      let parent = List.hd (stack t) |> Option.map ~f:(fun span -> span.id) in
      Recorded_event.span_begin
        ~sequence:t.sequence
        ~track:t.current_track
        ?source
        ~id
        ?parent
        ~fields:(fields ())
        ~name
        ()
      |> record t;
      set_stack t ({ id } :: stack t);
      match f () with
      | result ->
        set_stack t (List.tl_exn (stack t));
        Recorded_event.span_end
          ~sequence:t.sequence
          ~track:t.current_track
          ?source
          ~id
          ~name
          ()
        |> record t;
        result
      | exception exn ->
        set_stack t (List.tl_exn (stack t));
        Recorded_event.message
          ~sequence:t.sequence
          ~track:t.current_track
          ?source
          ~level:`Error
          ~fields:[ "exception", Sexp.Atom (Exn.to_string exn) ]
          (`String "span raised")
        |> record t;
        Recorded_event.span_end
          ~sequence:t.sequence
          ~track:t.current_track
          ?source
          ~id
          ~name
          ()
        |> record t;
        raise exn)
  ;;
end

module Event = struct
  let emit_message_data t ?(level = `Info) ?source ?(fields = fun () -> []) message =
    if would_log t level
    then
      Recorded_event.message
        ~sequence:t.sequence
        ~track:t.current_track
        ?source
        ~level
        ~fields:(fields ())
        message
      |> record t
  ;;

  let emit t ?level ?source ?fields message =
    emit_message_data t ?level ?source ?fields (`String message)
  ;;
end

module Track = struct
  let set t track =
    ensure_open t "Track.set";
    t.current_track <- track
  ;;

  let with_ t track ~f =
    let previous = t.current_track in
    set t track;
    Exn.protect ~f ~finally:(fun () -> set t previous)
  ;;
end
