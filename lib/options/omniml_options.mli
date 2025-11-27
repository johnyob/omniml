open Core

module type S = sig
  type t [@@deriving sexp]

  val arg_type : t Command.Arg_type.t
end

module type S_with_default = sig
  include S

  val default : t
end

module Defaulting : sig
  type t =
    | Disabled
    | Scc
  [@@deriving sexp]

  include S_with_default with type t := t
end
