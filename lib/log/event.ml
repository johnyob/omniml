open! Import

type field = string * Sexp.t [@@deriving sexp_of]

type t =
  { sequence : int
  ; track : int
  ; source : Source.t option
  ; desc : desc
  }

and desc =
  | Span_begin of
      { id : int
      ; parent : int option
      ; name : string
      ; fields : field list
      }
  | Span_end of
      { id : int
      ; name : string
      }
  | Message of
      { level : Level.t
      ; message : Sexp_or_string.t
      ; fields : field list
      }
[@@deriving sexp_of]

let create ~sequence ~track ?source desc = { sequence; track; source; desc }

let message_parts : Message_data.t -> Sexp_or_string.t * field list = function
  | `String string -> `String string, []
  | `Sexp sexp -> `Sexp sexp, []
  | `Structured message ->
    let label =
      match Ppx_log_types.Message_sexp.label message with
      | None -> `String "log"
      | Some (String string | String_literal string) -> `String string
    in
    let fields =
      Ppx_log_types.Message_sexp.tags message
      |> List.map ~f:(fun { Ppx_log_types.Log_tag.name; data } ->
        name, Ppx_log_types.Tag_data.Without_type_label.sexp_of_t data)
    in
    label, fields
;;

let message ~sequence ~track ?source ~level ?(fields = []) message_data =
  let message, message_fields = message_parts message_data in
  create
    ~sequence
    ~track
    ?source
    (Message { level; message; fields = fields @ message_fields })
;;

let span_begin ~sequence ~track ?source ~id ?parent ?(fields = []) ~name () =
  create ~sequence ~track ?source (Span_begin { id; parent; name; fields })
;;

let span_end ~sequence ~track ?source ~id ~name () =
  create ~sequence ~track ?source (Span_end { id; name })
;;

let sequence t = t.sequence
let track t = t.track
let source t = t.source
let desc t = t.desc
