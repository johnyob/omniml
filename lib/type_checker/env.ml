open! Import
open Ast_types
open Adt

module Var_binding = struct
  type t =
    { var : Constraint.Var.t option
    ; overloads : Constraint.Var.t Over_name.Map.t
    }
  [@@deriving sexp_of]

  let empty = { var = None; overloads = Over_name.Map.empty }
  let set t var = { t with var = Some var }

  let set_overload t over_name cvar =
    { t with overloads = Map.set t.overloads ~key:over_name ~data:cvar }
  ;;
end

type t =
  { id_source : Identifier.source
    (** [id_source] is the identifier source for any constraint variables
      created while generating constraints *)
  ; constrs : constructor_definition list Constructor_name.Map.t
    (** [constrs] is a map from (user-defined) constructor names to
      constructor_defition. Each definition has a unique [constr_ident]. *)
  ; labels : label_definition list Label_name.Map.t
  ; types : type_definition list Type_name.Map.t
    (** [types] is a map from (user-defined) type names to type definitions.
      Each definition has a unique [type_ident]. *)
  ; type_vars : Type.Var.t Type_var_name.Map.t
    (** [type_vars] is a renaming from (user-defined) type variables to
      constraint type variables (unique). *)
  ; vars : Var_binding.t Var_name.Map.t
    (** [vars] is a renaming from (user-defined) variable names to
      constraint variables (unique). *)
  }

let empty ?(id_source = Identifier.create_source ()) () =
  { id_source
  ; constrs = Constructor_name.Map.empty
  ; labels = Label_name.Map.empty
  ; type_vars = Type_var_name.Map.empty
  ; types = Type_name.Map.empty
  ; vars = Var_name.Map.empty
  }
;;

let id_source t = t.id_source [@@inline]

let add_constr_def t (constr_def : constructor_definition) =
  { t with
    constrs = Map.add_multi t.constrs ~key:constr_def.constructor_name ~data:constr_def
  }
;;

let add_label_def t (label_def : label_definition) =
  { t with labels = Map.add_multi t.labels ~key:label_def.label_name ~data:label_def }
;;

let add_type_def t type_def =
  let t =
    { t with types = Map.add_multi t.types ~key:type_def.type_name ~data:type_def }
  in
  match type_def.type_kind with
  | Type_variant constr_decls -> List.fold_left constr_decls ~init:t ~f:add_constr_def
  | Type_record label_defs -> List.fold_left label_defs ~init:t ~f:add_label_def
  | Type_abstract -> t
;;

let declare_type t ~type_name ~type_arity ~in_ =
  let type_ident =
    Type_ident.create ~id_source:t.id_source ~name:(type_name : Type_name.t :> string) ()
  in
  (* Declarations are nothing but abstract type definitions *)
  let decl = { type_name; type_ident; type_arity; type_kind = Type_abstract } in
  in_ (add_type_def t decl) type_ident
;;

let declare_types t types ~in_ =
  let decls =
    List.map types ~f:(fun (type_name, type_arity) ->
      { type_name
      ; type_ident =
          Type_ident.create
            ~id_source:t.id_source
            ~name:(type_name : Type_name.t :> string)
            ()
      ; type_arity
      ; type_kind = Type_abstract
      })
  in
  let type_idents = List.map decls ~f:(fun decl -> decl.type_ident) in
  in_ (List.fold decls ~init:t ~f:add_type_def) type_idents
;;

let find_constr t constr = Map.find_multi t.constrs constr
let find_label t label = Map.find_multi t.labels label
let find_type_def t type_name = Map.find_multi t.types type_name
let find_type_var t type_var = Map.find t.type_vars type_var
let find_var t var = Map.find t.vars var

let rename_type_var t ~type_var ~in_ =
  let ctype_var =
    Type.Var.create ~id_source:t.id_source ~name:(type_var : Type_var_name.t :> string) ()
  in
  let t = { t with type_vars = Map.set t.type_vars ~key:type_var ~data:ctype_var } in
  in_ t ctype_var
;;

let rename_var_binding ~name ~set_binding t ~var ~in_ =
  let cvar = Constraint.Var.create ~id_source:t.id_source ~name () in
  let t =
    { t with
      vars =
        Map.update t.vars var ~f:(fun binding ->
          let binding = Option.value binding ~default:Var_binding.empty in
          set_binding binding cvar)
    }
  in
  in_ t cvar
;;

let rename_var t ~var ~in_ =
  rename_var_binding
    ~name:(var : Var_name.t :> string)
    ~set_binding:Var_binding.set
    t
    ~var
    ~in_
;;

let rename_over_path t ~over_path:(over_name, var) ~in_ =
  rename_var_binding
    ~name:(Fmt.str "%a?%a" Over_name.pp over_name Var_name.pp var)
    ~set_binding:(fun binding cvar -> Var_binding.set_overload binding over_name cvar)
    t
    ~var
    ~in_
;;
