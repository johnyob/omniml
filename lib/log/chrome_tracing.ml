open! Import

let add_escaped_string buffer string =
  Buffer.add_char buffer '"';
  String.iter string ~f:(function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '\b' -> Buffer.add_string buffer "\\b"
    | '\012' -> Buffer.add_string buffer "\\f"
    | '\n' -> Buffer.add_string buffer "\\n"
    | '\r' -> Buffer.add_string buffer "\\r"
    | '\t' -> Buffer.add_string buffer "\\t"
    | character when Char.to_int character < 0x20 ->
      bprintf buffer "\\u%04x" (Char.to_int character)
    | character -> Buffer.add_char buffer character);
  Buffer.add_char buffer '"'
;;

let add_fields buffer fields =
  List.iteri fields ~f:(fun index (name, value) ->
    if index > 0 then Buffer.add_char buffer ',';
    add_escaped_string buffer name;
    Buffer.add_char buffer ':';
    add_escaped_string buffer (Sexp.to_string value))
;;

let source_fields = function
  | None | Some (Source.Manually_constructed _) -> []
  | Some (Code { pos_fname; pos_lnum; library_name }) ->
    [ "source.file", Sexp.Atom pos_fname
    ; "source.line", [%sexp (pos_lnum : int)]
    ; "source.library", Sexp.Atom library_name
    ]
;;

let add_common buffer event =
  bprintf
    buffer
    ",\"ts\":%d,\"pid\":0,\"tid\":%d"
    (Event.sequence event)
    (Event.track event)
;;

let add_args buffer fields =
  if not (List.is_empty fields)
  then (
    Buffer.add_string buffer ",\"args\":{";
    add_fields buffer fields;
    Buffer.add_char buffer '}')
;;

let add_event buffer event =
  let source = Event.source event in
  Buffer.add_char buffer '{';
  (match Event.desc event with
   | Event.Span_begin { id; parent; name; fields } ->
     Buffer.add_string buffer "\"name\":";
     add_escaped_string buffer name;
     Buffer.add_string buffer ",\"cat\":\"span\",\"ph\":\"B\"";
     add_common buffer event;
     add_args
       buffer
       ([ "span.id", [%sexp (id : int)]; "span.parent", [%sexp (parent : int option)] ]
        @ source_fields source
        @ fields)
   | Span_end { id; name } ->
     Buffer.add_string buffer "\"name\":";
     add_escaped_string buffer name;
     Buffer.add_string buffer ",\"cat\":\"span\",\"ph\":\"E\"";
     add_common buffer event;
     add_args buffer [ "span.id", [%sexp (id : int)] ]
   | Message { level; message; fields } ->
     Buffer.add_string buffer "\"name\":";
     add_escaped_string buffer (Sexp_or_string.to_string message);
     Buffer.add_string buffer ",\"cat\":\"log\",\"ph\":\"i\",\"s\":\"t\"";
     add_common buffer event;
     add_args
       buffer
       ([ "level", Sexp.Atom (Level.to_string level) ] @ source_fields source @ fields));
  Buffer.add_char buffer '}'
;;

let to_string log =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer "{\"displayTimeUnit\":\"ms\",\"traceEvents\":[";
  List.iteri (Log.to_list log) ~f:(fun index event ->
    if index > 0 then Buffer.add_char buffer ',';
    add_event buffer event);
  Buffer.add_string buffer "]}\n";
  Buffer.contents buffer
;;

let write_channel channel log =
  Out_channel.output_string channel (to_string log);
  Out_channel.flush channel
;;

let write_file path log = Out_channel.write_all path ~data:(to_string log)
