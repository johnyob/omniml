open! Import
module G = Generalization

module Var = Var.Make (struct
    let module_name = "Decoded_type.Var"
  end)

type t =
  | Var of Var.t
  | Arrow of t * t
  | Tuple of t list
  | Constr of t list * Type.Ident.t
  | Poly of scheme
  | Mu of Var.t * t

and scheme =
  { quantifiers : Var.t list
  ; body : t
  }
[@@deriving sexp]

module Scheme = struct
  type nonrec t = scheme =
    { quantifiers : Var.t list
    ; body : t
    }
  [@@deriving sexp]

  let create ?(quantifiers = []) body = { quantifiers; body }
end

module Decoder = struct
  module State = struct
    type t =
      { id_source : Identifier.source
        (** An identifier source used to allocate variables *)
      ; variable_renaming : (Identifier.t, Var.t) Hashtbl.t
        (** A mapping from variable structure identifiers to allocated variables *)
      }

    let create () =
      { id_source = Identifier.create_source ()
      ; variable_renaming = Hashtbl.create (module Identifier)
      }
    ;;

    let alloc_var t = Var.create ~id_source:t.id_source ()

    let rename_var t id =
      Hashtbl.find_or_add t.variable_renaming id ~default:(fun () -> alloc_var t)
    ;;
  end

  type nonrec t = G.Type.t -> t

  type status =
    | Active (** A node is actively being visited. *)
    | Cyclical of Var.t
    (** A cyclical node with an allocated variable (for a mu-binder). *)
  [@@deriving sexp_of]

  let create () : t =
    let state = State.create () in
    fun gtype ->
      let visited_table = Hashtbl.create (module Identifier) in
      (* Recursive loop that traverses the graphical type *)
      let rec decode_shape args = function
        | Principal_shape.Sh_arrow ->
          (match args with
           | [ param; return ] -> Arrow (param, return)
           | _ -> assert false)
        | Principal_shape.Sh_tuple _ -> Tuple args
        | Principal_shape.Sh_constr (_, constr) -> Constr (args, constr)
        | Principal_shape.Sh_poly poly_shape -> Poly (decode_poly_shape args poly_shape)
      and decode_poly_shape args ({ quantifiers; scheme } : Principal_shape.Poly.t) =
        decode_scheme (List.zip_exn quantifiers args) scheme
      and decode_scheme substitution ({ quantifiers; body } : Type.Scheme.t) =
        let quantifiers =
          List.map quantifiers ~f:(fun quantifier -> quantifier, State.alloc_var state)
        in
        let body =
          let quantified_variables =
            List.map quantifiers ~f:(fun (quantifier, var) -> quantifier, Var var)
          in
          decode_constraint_type (quantified_variables @ substitution) body
        in
        { quantifiers = List.map quantifiers ~f:snd; body }
      and decode_constraint_type substitution (type_ : Type.t) =
        match type_ with
        | Type.Var var ->
          (match
             List.find_map substitution ~f:(fun (quantifier, type_) ->
               Option.some_if (Type.Var.equal quantifier var) type_)
           with
           | Some type_ -> type_
           | None -> Var (State.rename_var state var.id))
        | Type.Arrow (param, return) ->
          Arrow
            ( decode_constraint_type substitution param
            , decode_constraint_type substitution return )
        | Type.Tuple types ->
          Tuple (List.map types ~f:(decode_constraint_type substitution))
        | Type.Constr (types, constr) ->
          Constr (List.map types ~f:(decode_constraint_type substitution), constr)
        | Type.Shape (types, shape) ->
          decode_shape (List.map types ~f:(decode_constraint_type substitution)) shape
        | Type.Poly scheme ->
          let args, poly_shape = Principal_shape.poly_shape_decomposition_of_scheme scheme in
          let args = List.map args ~f:(decode_constraint_type substitution) in
          decode_shape args (Principal_shape.Sh_poly poly_shape)
      and decode type_ =
        let structure = G.Type.structure type_ in
        let id = structure.id in
        match Hashtbl.find visited_table id with
        | Some (Cyclical var) ->
          (* Node is cyclic, use allocated variable *)
          Var var
        | Some Active ->
          let var = State.alloc_var state in
          (* Mark the node as being cyclic.
             Allocate a variable to represent cyclic positions *)
          Hashtbl.set visited_table ~key:id ~data:(Cyclical var);
          Var var
        | None ->
          (* Mark the node as being visited *)
          Hashtbl.set visited_table ~key:id ~data:Active;
          (* Visit children *)
          let result = decode_first_order_structure ~id structure.inner in
          (* Safety: Cannot through an exception since the visited table
             must have an entry for this node. *)
          let status = Hashtbl.find_exn visited_table id in
          Hashtbl.remove visited_table id;
          (match status with
           | Cyclical var -> Mu (var, result)
           | Active -> result)
      and decode_first_order_structure ~id structure =
        match structure with
        | Var -> Var (State.rename_var state id)
        | Structure s -> decode_rigid_structure ~id s
      and decode_rigid_structure ~id structure =
        match structure with
        | Rigid_var -> Var (State.rename_var state id)
        | Structure s -> decode_suspended_structure ~id s
      and decode_suspended_structure ~id structure =
        match structure with
        | Shape_app { args; shape_var } ->
          (match G.Type.inner args, Principal_shape.Var.peek_exn shape_var with
           | Structure (Structure (Shape_args args)), shape ->
             let args = List.map args ~f:decode in
             decode_shape args shape
           | _ -> Var (State.rename_var state id)
           | exception Principal_shape.Var.Empty -> Var (State.rename_var state id))
        | Shape_args _ ->
          (* Kind error, expected type *)
          Omniml_error.(
            raise
            @@ bug_s
                 ~here:[%here]
                 [%message
                   "Kind error when decoding types. Expected type, got args."
                     (id : Identifier.t)
                     (structure : G.Type.t G.M.t)])
        | Structure f -> decode_former f
      and decode_former { args; shape } =
        let args = List.map args ~f:decode in
        decode_shape args shape
      in
      decode gtype
  ;;
end
