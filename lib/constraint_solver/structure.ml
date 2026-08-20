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
    | Shape_var of Principal_shape.Var.t
    | Structure of 'a S.t
  [@@deriving sexp_of]

  exception Cannot_merge = S.Cannot_merge

  type 'a ctx =
    { super : 'a S.ctx
    ; shape_of_structure : 'a S.t -> Principal_shape.t option
    ; scheduler : Scheduler.t
    }

  let iter t ~f =
    match t with
    | Shape_var _ -> ()
    | Structure s -> S.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Shape_var _shape_var -> init
    | Structure s -> S.fold s ~f ~init
  ;;

  let map t ~f =
    match t with
    | Shape_var shape_var -> Shape_var shape_var
    | Structure s -> Structure (S.map s ~f)
  ;;

  let unify_svar svar1 svar2 =
    Principal_shape.Var.(
      try unify svar1 svar2 with
      | Unify _ -> raise Cannot_merge)
  ;;

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    match t1, t2 with
    | Shape_var shape_var1, Shape_var shape_var2 ->
      (try Principal_shape.Var.unify ~scheduler:ctx.scheduler shape_var1 shape_var2 with
       | Principal_shape.Var.Unify _ -> raise Cannot_merge);
      t1
    | (Structure s as t), Shape_var shape_var | Shape_var shape_var, (Structure s as t) ->
      ctx.shape_of_structure s
      |> Option.iter ~f:(fun shape ->
        try Principal_shape.Var.fill_exn ~scheduler:ctx.scheduler shape_var shape with
        | Principal_shape.Var.Not_empty -> raise Cannot_merge);
      t
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
