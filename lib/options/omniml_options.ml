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
      | Unary
    [@@deriving sexp, enumerate]
  end

  include T

  let default = Disabled
  let arg_type = Command.Arg_type.enumerated_sexpable ~case_sensitive:false (module T)
end

module Termination_check = struct
  module T = struct
    type t =
      | Disabled
      | Threshold of int
      | Decreasing_instantiations
    [@@deriving sexp]
  end

  include T

  let default = Threshold 256

  let arg_type =
    Command.Arg_type.create (fun arg_str ->
      match String.lowercase arg_str with
      | "disabled" -> Disabled
      | "decreasing-instantiations" -> Decreasing_instantiations
      | s when String.is_prefix s ~prefix:"threshold:" ->
        let n = String.drop_prefix s 10 |> Int.of_string in
        Threshold n
      | _ ->
        Error.(
          raise
            (of_string
               "expected one of: disabled, threshold:<int>, decreasing-instantiations")))
  ;;
end
