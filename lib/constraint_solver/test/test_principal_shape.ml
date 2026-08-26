open Core
open Omniml_constraint_solver.For_testing

let%quick_test _ =
  fun (type_scheme :
        (Type.Scheme.t
        [@generator Quickcheckable.Type.Scheme.quickcheck_generator]
        [@shrinker Quickcheckable.Type.Scheme.quickcheck_shrinker])) ->
  let _, scheme_shape = Principal_shape.scheme_shape_decomposition type_scheme in
  Principal_shape.Scheme.invariant scheme_shape
;;
