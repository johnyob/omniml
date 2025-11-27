open Core

module type S = sig
  type t [@@deriving sexp]

  val arg_type : t Command.Arg_type.t
end

module type S_with_default = sig
  include S

  val default : t
end

module Defaulting = struct
  module T = struct
    type t =
      | Disabled
      | Scc
    [@@deriving sexp, enumerate]
  end

  include T

  let default = Disabled
  let arg_type = Command.Arg_type.enumerated_sexpable (module T)
end
