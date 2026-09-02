module Source = Ppx_log_types.Message_source
module Message_data = Ppx_log_types.Message_data
module Level = Level
module Sexp_or_string = Sexp_or_string
module Event = Event
module Log = Log
module Chrome_tracing = Chrome_tracing
module Global = Global
module Ppx_log_syntax = Ppx_log_syntax

module type S = sig
  type 'a t_to_fun
  type 'a t_to_val

  val level : Level.t t_to_val
  val set_level : (Level.t -> unit) t_to_fun
  val set_output : (Out_channel.t option -> unit) t_to_fun
  val would_log : (Level.t -> bool) t_to_fun
  val clear : unit t_to_val
  val close : unit t_to_val

  module Span : sig
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
    val with_ : (int -> f:(unit -> 'a) -> 'a) t_to_fun
  end
end
