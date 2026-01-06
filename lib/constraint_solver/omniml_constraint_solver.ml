open! Import
module Type = Type
module Constraint = Constraint
module Decoded_type = Decoded_type
module Error = Solver.Error

let solve = Solver.solve

module For_testing = struct
  module Types = Types
  module Type = Type
  module Principal_shape = Principal_shape

  module Quickcheckable = struct
    [@@@warning "-30"]

    type type_ = Type.t =
      | Var of Type.Var.t
      | Arrow of type_ * type_
      | Tuple of type_ list
      | Constr of type_ list * Type.Ident.t
      | Shape of type_ list * principal_shape
      | Poly of type_scheme
    [@@deriving quickcheck]

    and poly_principal_shape = Principal_shape.Poly.t =
      { quantifiers : Type.Var.t list
      ; scheme : type_scheme
      }
    [@@deriving quickcheck]

    and type_scheme = Type.Scheme.t =
      { quantifiers : Type.Var.t list
      ; body : type_
      }
    [@@deriving quickcheck]

    and principal_shape = Principal_shape.t =
      | Sh_arrow [@quickcheck.do_not_generate]
      | Sh_tuple of int
      | Sh_constr of int * Type.Ident.t
      | Sh_poly of poly_principal_shape [@quickcheck.do_not_generate]
    [@@deriving quickcheck]

    module Type = struct
      let quickcheck_generator = quickcheck_generator_type_
      let quickcheck_shrinker = quickcheck_shrinker_type_
      let quickcheck_observer = quickcheck_observer_type_

      module Scheme = struct
        let quickcheck_generator = quickcheck_generator_type_scheme
        let quickcheck_shrinker = quickcheck_shrinker_type_scheme
        let quickcheck_observer = quickcheck_observer_type_scheme
      end
    end

    module Principal_shape = struct
      (* We don't generate poly or arrow shapes due to the invariants that come with them. *)
      let quickcheck_generator = quickcheck_generator_principal_shape
      let quickcheck_shrinker = quickcheck_shrinker_principal_shape
      let quickcheck_observer = quickcheck_observer_principal_shape
    end
  end
end
