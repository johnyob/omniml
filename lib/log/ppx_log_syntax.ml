open! Import

let level_of_ppx : Ppx_log_types.Level.t -> Level.t = function
  | `Debug -> `Debug
  | `Info -> `Info
  | `Error -> `Error
;;

type t = Log.t
type time = unit
type return_type = unit

let would_log log level =
  Log.would_log log (Option.value_map level ~default:`Info ~f:level_of_ppx)
;;

let default = ()

let message ?(level = `Info) ?time:_ ?(tags = []) log data source =
  let fields () = List.map tags ~f:(Tuple2.map_snd ~f:(fun value -> Sexp.Atom value)) in
  Log.Event.emit_message_data log ~level:(level_of_ppx level) ~source ~fields data
;;

module Global = struct
  type return_type = unit

  let would_log level =
    Global.would_log (Option.value_map level ~default:`Info ~f:level_of_ppx)
  ;;

  let default = ()

  let message ?(level = `Info) ?time:_ ?(tags = []) data source =
    let fields () = List.map tags ~f:(Tuple2.map_snd ~f:(fun value -> Sexp.Atom value)) in
    Log.Event.emit_message_data
      (Global.get ())
      ~level:(level_of_ppx level)
      ~source
      ~fields
      data
  ;;
end
