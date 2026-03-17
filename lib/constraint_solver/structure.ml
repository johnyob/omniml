open! Import

module type S = Omniml_unifier.Structure.S

module Former = struct
  type 'a t =
    { args : 'a list
    ; shape : Principal_shape.t
    }
  [@@deriving sexp]

  type 'a ctx = unit

  exception Cannot_merge

  let iter { args; shape = _ } ~f = List.iter args ~f
  let map { args; shape } ~f = { args = List.map args ~f; shape }
  let fold { args; shape = _ } ~f ~init = List.fold_right args ~f ~init

  let merge
        ~ctx:()
        ~create:_
        ~unify
        ~type1:_
        ~type2:_
        ({ args = args1; shape = s1 } as t)
        { args = args2; shape = s2 }
    =
    if Principal_shape.(s1 = s2)
    then (
      match List.iter2 args1 args2 ~f:unify with
      | Ok () -> t
      | Unequal_lengths -> raise Cannot_merge)
    else raise Cannot_merge
  ;;
end

module Rigid (S : S) = struct
  type 'a t =
    | Rigid_var
    | Structure of 'a S.t
  [@@deriving sexp_of]

  type 'a ctx = 'a S.ctx

  exception Cannot_merge = S.Cannot_merge

  let iter t ~f =
    match t with
    | Rigid_var -> ()
    | Structure s -> S.iter s ~f
  ;;

  let map t ~f =
    match t with
    | Rigid_var -> Rigid_var
    | Structure s -> Structure (S.map s ~f)
  ;;

  let fold t ~f ~init =
    match t with
    | Rigid_var -> init
    | Structure s -> S.fold s ~f ~init
  ;;

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    match t1, t2 with
    | Rigid_var, _ | _, Rigid_var -> raise Cannot_merge
    | Structure s1, Structure s2 ->
      Structure
        (S.merge ~ctx ~create:(fun s -> create (Structure s)) ~unify ~type1 ~type2 s1 s2)
  ;;
end

module Shape_var (S : S) = struct
  type 'a t =
    | Shape_app of
        { args : 'a
        ; shape_var : Principal_shape.Var.t
        }
    | Shape_args of 'a list
    | Structure of 'a S.t
  [@@deriving sexp_of]

  exception Cannot_merge = S.Cannot_merge

  type 'a ctx =
    { super : 'a S.ctx
    ; decomposition_of_structure : 'a S.t -> 'a list * Principal_shape.t
    ; shape_var_state : Principal_shape.Var.State.t
    }

  let iter t ~f =
    match t with
    | Shape_app { args; shape_var = _ } -> f args
    | Shape_args ts -> List.iter ts ~f
    | Structure s -> S.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Shape_app { args; shape_var = _ } -> f args init
    | Shape_args ts -> List.fold_right ts ~f ~init
    | Structure s -> S.fold s ~f ~init
  ;;

  let map t ~f =
    match t with
    | Shape_app { args; shape_var } -> Shape_app { args = f args; shape_var }
    | Shape_args ts -> Shape_args (List.map ts ~f)
    | Structure s -> Structure (S.map s ~f)
  ;;

  let unify_svar svar1 svar2 =
    Principal_shape.Var.(
      try unify svar1 svar2 with
      | Unify _ -> raise Cannot_merge)
  ;;

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    match t1, t2 with
    | ( Shape_app { args = args1; shape_var = svar1 }
      , Shape_app { args = args2; shape_var = svar2 } ) ->
      Principal_shape.Var.(
        try unify ~state:ctx.shape_var_state svar1 svar2 with
        | Unify _ -> raise Cannot_merge);
      unify args1 args2;
      t1
    | (Structure s as t), Shape_app { args; shape_var }
    | Shape_app { args; shape_var }, (Structure s as t) ->
      let args', shape = ctx.decomposition_of_structure s in
      Principal_shape.Var.(
        try fill_exn ~state:ctx.shape_var_state shape_var shape with
        | Not_empty -> raise Cannot_merge);
      unify args (create (Shape_args args'));
      t
    | Shape_args args1, Shape_args args2 ->
      (try List.iter2_exn args1 args2 ~f:unify with
       | _ -> raise Cannot_merge);
      t1
    | (Shape_app _ | Structure _), Shape_args _ | Shape_args _, (Shape_app _ | Structure _)
      -> raise Cannot_merge
    | Structure s1, Structure s2 ->
      Structure
        (S.merge
           ~ctx:ctx.super
           ~create:(fun s -> create (Structure s))
           ~unify
           ~type1
           ~type2
           s1
           s2)
  ;;
end

module First_order (S : S) = struct
  type 'a t =
    | Var
    | Structure of 'a S.t
  [@@deriving sexp_of]

  exception Cannot_merge = S.Cannot_merge

  type 'a ctx = 'a S.ctx

  let iter t ~f =
    match t with
    | Var -> ()
    | Structure s -> S.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Var -> init
    | Structure s -> S.fold s ~f ~init
  ;;

  let map t ~f =
    match t with
    | Var -> Var
    | Structure s -> Structure (S.map s ~f)
  ;;

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    match t1, t2 with
    | Var, t | t, Var -> t
    | Structure s1, Structure s2 ->
      Structure
        (S.merge ~ctx ~create:(fun s -> create (Structure s)) ~unify ~type1 ~type2 s1 s2)
  ;;

  let is_var t =
    match t with
    | Var -> true
    | Structure _ -> false
  ;;
end
