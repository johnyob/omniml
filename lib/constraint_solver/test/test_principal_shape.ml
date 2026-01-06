open Core
open Omniml_constraint_solver.For_testing

let%quick_test _ =
  fun (type_scheme :
        (Type.Scheme.t
        [@generator Quickcheckable.Type.Scheme.quickcheck_generator]
        [@shrinker Quickcheckable.Type.Scheme.quickcheck_shrinker])) ->
  let _, poly_shape = Principal_shape.poly_shape_decomposition_of_scheme type_scheme in
  Principal_shape.Poly.invariant poly_shape
;;
