open! Core

module T = struct
  type t =
    [ `Trace
    | `Debug
    | `Info
    | `Warn
    | `Error
    ]
  [@@deriving compare, equal, sexp, enumerate]

  let severity = function
    | `Trace -> 0
    | `Debug -> 1
    | `Info -> 2
    | `Warn -> 3
    | `Error -> 4
  ;;

  let compare_by_severity t1 t2 = Int.compare (severity t1) (severity t2)

  let to_string = function
    | `Trace -> "trace"
    | `Debug -> "debug"
    | `Info -> "info"
    | `Warn -> "warn"
    | `Error -> "error"
  ;;

  let of_string string =
    match String.lowercase string with
    | "trace" -> `Trace
    | "debug" -> `Debug
    | "info" -> `Info
    | "warn" | "warning" -> `Warn
    | "error" -> `Error
    | _ -> failwithf "unknown log level %S" string ()
  ;;
end

include T

let arg_type : t Command.Arg_type.t =
  Command.Arg_type.enumerated
    ~list_values_in_help:true
    ~case_sensitive:false
    (module T : Command.Enumerable_stringable with type t = t)
;;
