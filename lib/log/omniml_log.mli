open! Core

(** Synchronous structured logging and execution tracing for OmniML. *)

(** PPX-compatible event source information. *)
module Source = Ppx_log_types.Message_source

(** Message payloads produced by [ppx_log]. *)
module Message_data = Ppx_log_types.Message_data

(** Event severity, ordered from [`Trace] through [`Error]. A log accepts an event when
    the event's severity is at least the log's configured level. *)
module Level : sig
  type t =
    [ `Trace
    | `Debug
    | `Info
    | `Warn
    | `Error
    ]
  [@@deriving enumerate, compare, equal, sexp]

  include Stringable.S with type t := t

  val compare_by_severity : t -> t -> int
  val arg_type : t Command.Arg_type.t
end

(** The display payload of a message event. Structured fields are represented
    separately as [Sexp.t] values. *)
module Sexp_or_string : sig
  type t =
    [ `Sexp of Sexp.t
    | `String of string
    ]
  [@@deriving sexp]

  val to_string : t -> string
end

(** An immutable event recorded by a {!Log.t}. *)
module Event : sig
  type field = string * Sexp.t [@@deriving sexp_of]
  type t [@@deriving sexp_of]

  (** Constructs a message event. This is primarily useful for importers and tests;
      normal logging should use {!Log.Event.emit}. *)
  val message
    :  sequence:int
    -> track:int
    -> ?source:Source.t
    -> level:Level.t
    -> ?fields:field list
    -> Message_data.t
    -> t

  (** Constructs the first event of a span. [id] must be unique within its log. *)
  val span_begin
    :  sequence:int
    -> track:int
    -> ?source:Source.t
    -> id:int
    -> ?parent:int
    -> ?fields:field list
    -> name:string
    -> unit
    -> t

  (** Constructs the final event of a span. *)
  val span_end
    :  sequence:int
    -> track:int
    -> ?source:Source.t
    -> id:int
    -> name:string
    -> unit
    -> t

  (** Zero-based recording order. Closing a log does not change event sequences. *)
  val sequence : t -> int

  (** Logical track, exported as the Chrome trace thread ID. *)
  val track : t -> int

  val source : t -> Source.t option

  (** The inspectable event payload. Span IDs are unique within one log. A span begin
      records its parent explicitly, allowing consumers to inspect nesting without
      reconstructing it from event order. *)
  type desc =
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

  val desc : t -> desc
end

(** Operations shared by an explicitly owned {!Log.t} and the implicit {!Global} log.

    [with_t_fun] wraps an operation which already accepts arguments. [with_t_val] wraps
    an operation with no arguments beyond selecting the log. Their concrete definitions
    let [Log] prepend an explicit [Log.t], while [Global] retains conventional nullary
    functions such as [Global.level ()]. *)
module type S = sig
  type 'a t_to_fun
  type 'a t_to_val

  val level : Level.t t_to_val
  val set_level : (Level.t -> unit) t_to_fun

  (** Changes synchronous text output. [None] disables it without affecting recording. *)
  val set_output : (Out_channel.t option -> unit) t_to_fun

  (** An inexpensive gate which is false when [level] is disabled or the log is closed. *)
  val would_log : (Level.t -> bool) t_to_fun

  (** Removes recorded events and resets IDs. The log must be open with no active span. *)
  val clear : unit t_to_val

  (** Closes the selected log. This is idempotent, but raises [Invalid_argument] if a
      span is active. *)
  val close : unit t_to_val

  module Span : sig
    (** Records a begin/end pair around [f ()]. If [f] raises, an error message and the
        matching end are recorded before the exception is re-raised.

        [fields] is evaluated only when [level] is enabled. The default is [`Debug]. *)
    val with_
      : (?level:Level.t
         -> ?source:Source.t
         -> ?fields:(unit -> Event.field list)
         -> string
         -> f:(unit -> 'a)
         -> 'a)
          t_to_fun
  end

  module Event : sig
    (** Records an instantaneous string message. [fields] is evaluated only when [level]
        is enabled. The default is [`Info]. *)
    val emit
      : (?level:Level.t
         -> ?source:Source.t
         -> ?fields:(unit -> Event.field list)
         -> string
         -> unit)
          t_to_fun
  end

  module Track : sig
    val set : (int -> unit) t_to_fun

    (** Temporarily selects a track and restores the previous one after return or
        exception. *)
    val with_ : (int -> f:(unit -> 'a) -> 'a) t_to_fun
  end
end

(** A mutable structured event recorder.

    A log is intended for single-domain use: recording, configuration changes, closing,
    and text output are not synchronized. Calling {!close} freezes the event sequence.
    Operations which mutate a closed log raise [Invalid_argument]; attempts to emit
    events after closing are ignored. *)
module Log : sig
  type t [@@deriving sexp_of]

  (** Creates an open log. It accepts [`Info] and more severe events by default. When
      [output] is supplied, accepted events are also written and flushed synchronously in
      a compact tree-oriented text form. *)
  val create : ?level:Level.t -> ?output:Out_channel.t -> unit -> t

  val is_closed : t -> bool

  (** Returns recorded events in order. The log must be closed. *)
  val to_list : t -> Event.t list

  (** These inspection functions require a closed log. *)
  val length : t -> int

  val is_empty : t -> bool

  include S with type 'a t_to_fun := t -> 'a and type 'a t_to_val := t -> 'a
end

(** Synchronous Chrome Trace Event Format serialization. Every operation requires a
    closed {!Log.t} and raises [Invalid_argument] for an open log. *)
module Chrome_tracing : sig
  val to_string : Log.t -> string
  val write_channel : Out_channel.t -> Log.t -> unit

  (** Creates or replaces [path] and writes the complete log before returning. *)
  val write_file : string -> Log.t -> unit
end

(** The process-wide log used by [[%log.global]] sites. The initial log accepts [`Info]
    events and mirrors them synchronously to stderr. *)
module Global : sig
  include S with type 'a t_to_fun := 'a and type 'a t_to_val := unit -> 'a

  val get : unit -> Log.t
  val set : Log.t -> unit

  (** Installs a fresh default log. This does not change the configured trace file. *)
  val reset : unit -> unit

  (** Returns the current mutable log; this does not close or copy it. *)
  val log : unit -> Log.t

  (** Selects the file written by {!flush} and at normal process exit. *)
  val set_trace_file : string option -> unit

  (** Closes the current log and writes it synchronously when a trace file is configured. *)
  val flush : unit -> unit

  val set_level_via_param : unit -> unit Command.Param.t
  val set_trace_file_via_param : unit -> unit Command.Param.t

  module For_testing : sig
    val use_test_output : unit -> unit
  end
end

(** Adapter expected by Jane Street's [[%log]] PPX. [open Omniml_log] brings this module
    into the scope searched by the PPX. *)
module Ppx_log_syntax :
  Ppx_log_types.S
  with type t = Log.t
   and type time = unit
   and type return_type = unit
   and type Global.return_type = unit
