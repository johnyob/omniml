open Core
open Omniml_ast.Ast_types
module Ast = Omniml_ast.Ast

module Type = struct
  include Omniml_constraint_solver.Decoded_type

  let id_to_var_name (id : Omniml_std.Identifier.t) =
    let id = (id :> int) in
    let char = String.make 1 (Char.of_int_exn (Char.to_int 'a' + (id mod 26))) in
    let suffix = id / 26 in
    if suffix = 0 then char else char ^ Int.to_string suffix
  ;;

  let pp_var ppf (var : Var.t) = Fmt.pf ppf "'%s" (id_to_var_name var.id)

  let pp_ident ppf (ident : Omniml_constraint_solver.Type.Ident.t) =
    Fmt.string ppf (String.split_on_chars ~on:[ '.' ] ident.name |> List.last_exn)
  ;;

  let pp ~with_poly_params ppf type_ =
    let rec pp_mu ppf = function
      | Mu (var, type_) -> Fmt.pf ppf "@[%a@ as %a@]" pp_mu type_ pp_var var
      | type_ -> pp_arrow ppf type_
    and pp_arrow ppf = function
      | Arrow (param, return) ->
        let pp_param = if with_poly_params then pp_poly_param else pp_tuple in
        let pp_return = if with_poly_params then pp_poly_return else pp_arrow in
        Fmt.pf ppf "@[%a ->@ %a@]" pp_param param pp_return return
      | type_ -> pp_tuple ppf type_
    and pp_tuple ppf = function
      | Tuple types -> Fmt.pf ppf "@[%a@]" Fmt.(list ~sep:(any " *@ ") pp_app) types
      | type_ -> pp_app ppf type_
    and pp_app ppf = function
      | Constr (types, ident) -> Fmt.pf ppf "@[%a%a@]" pp_args types pp_ident ident
      | type_ -> pp_atom ppf type_
    and pp_args ppf = function
      | [] -> ()
      | [ type_ ] -> Fmt.pf ppf "%a@ " pp_app type_
      | types -> Fmt.pf ppf "@[(%a)@ @]" Fmt.(list ~sep:comma pp_mu) types
    and pp_atom ppf = function
      | Var var -> pp_var ppf var
      | Poly scheme -> Fmt.pf ppf "@[[%a]@]" pp_scheme scheme
      | (Arrow _ | Tuple _ | Constr _ | Mu _) as type_ -> Fmt.parens pp_mu ppf type_
    and pp_poly_param ppf = function
      | Poly scheme -> pp_param_scheme ppf scheme
      | type_ -> pp_tuple ppf type_
    and pp_poly_return ppf = function
      | Poly scheme -> pp_return_scheme ppf scheme
      | type_ -> pp_arrow ppf type_
    and pp_param_scheme ppf ({ quantifiers; body } as scheme) =
      match quantifiers with
      | [] -> pp_tuple ppf body
      | _ -> Fmt.parens pp_scheme ppf scheme
    and pp_return_scheme ppf ({ quantifiers; body } as scheme) =
      match quantifiers with
      | [] -> pp_arrow ppf body
      | _ -> Fmt.parens pp_scheme ppf scheme
    and pp_scheme ppf { quantifiers; body } =
      match quantifiers with
      | [] -> pp_mu ppf body
      | quantifiers ->
        Fmt.pf
          ppf
          "@[<hov 2>forall %a.@ %a@]"
          Fmt.(list ~sep:sp pp_var)
          quantifiers
          pp_mu
          body
    in
    pp_mu ppf type_
  ;;
end

type binding =
  { binding_name : Var_name.t
  ; binding_type : Type.t
  }
[@@deriving sexp_of]

type signature_item = signature_item_desc With_range.t

and signature_item_desc =
  | Sig_value of binding list
  | Sig_primitive of binding
  | Sig_type of Omniml_ast.Ast.type_declaration list
[@@deriving sexp_of]

type signature = signature_item list [@@deriving sexp_of]

let pp_var_name ppf name = Fmt.string ppf (Var_name.to_string name)
let pp_type_name ppf name = Fmt.string ppf (Type_name.to_string name)
let pp_constructor_name ppf name = Fmt.string ppf (Constructor_name.to_string name)
let pp_label_name ppf name = Fmt.string ppf (Label_name.to_string name)
let pp_type_var ppf type_var = Fmt.pf ppf "'%s" (Type_var_name.to_string type_var)

let rec pp_core_type ppf (type_ : Ast.core_type) =
  match type_.it with
  | Type_var type_var -> pp_type_var ppf type_var.it
  | Type_arrow (param, return) ->
    Fmt.pf ppf "@[%a ->@ %a@]" pp_param_type param pp_return_type return
  | Type_tuple types -> Fmt.pf ppf "@[%a@]" Fmt.(list ~sep:(any " *@ ") pp_app_type) types
  | Type_constr (args, type_name) -> pp_type_constr ppf (args, type_name)
  | Type_poly scheme -> Fmt.pf ppf "[%a]" pp_core_scheme scheme

and pp_param_type ppf ({ it; _ } : Ast.param_type) =
  match it with
  | Param_mono_type type_ ->
    (match type_.it with
     | Type_arrow _ -> Fmt.parens pp_core_type ppf type_
     | _ -> pp_core_type ppf type_)
  | Param_poly_type scheme -> Fmt.pf ppf "(forall %a)" pp_core_scheme scheme

and pp_return_type ppf ({ it; _ } : Ast.return_type) =
  match it with
  | Return_mono_type type_ -> pp_core_type ppf type_
  | Return_poly_type scheme -> Fmt.pf ppf "(forall %a)" pp_core_scheme scheme

and pp_app_type ppf (type_ : Ast.core_type) =
  match type_.it with
  | Type_var _ | Type_constr _ | Type_poly _ -> pp_core_type ppf type_
  | Type_arrow _ | Type_tuple _ -> Fmt.parens pp_core_type ppf type_

and pp_type_constr ppf (args, type_name) =
  match args with
  | [] -> pp_type_name ppf type_name.it
  | [ arg ] -> Fmt.pf ppf "@[%a@ %a@]" pp_app_type arg pp_type_name type_name.it
  | args ->
    Fmt.pf
      ppf
      "@[(%a)@ %a@]"
      Fmt.(list ~sep:(any ",@ ") pp_core_type)
      args
      pp_type_name
      type_name.it

and pp_core_scheme ppf ({ it = { scheme_quantifiers; scheme_body }; _ } : Ast.core_scheme)
  =
  match scheme_quantifiers with
  | [] -> pp_core_type ppf scheme_body
  | quantifiers ->
    Fmt.pf
      ppf
      "@[%a.@ %a@]"
      Fmt.(list ~sep:sp (using With_range.it pp_type_var))
      quantifiers
      pp_core_type
      scheme_body
;;

let pp_binding ~with_poly_params keyword ppf { binding_name; binding_type } =
  Fmt.pf
    ppf
    "@[<hov 2>%s %a :@ %a@]"
    keyword
    pp_var_name
    binding_name
    (Type.pp ~with_poly_params)
    binding_type
;;

let pp_type_params ppf = function
  | [] -> ()
  | [ param ] -> Fmt.pf ppf "%a " pp_type_var (With_range.it param)
  | params ->
    Fmt.pf ppf "(%a) " Fmt.(list ~sep:(any ", ") (using With_range.it pp_type_var)) params
;;

let pp_type_decl_kind ppf = function
  | Ast.Type_decl_abstract -> ()
  | Type_decl_variant constructors ->
    let pp_constructor ppf { Ast.constructor_name; constructor_arg } =
      Fmt.pf
        ppf
        "@[<hov 2>| %a%a@]"
        pp_constructor_name
        constructor_name.it
        Fmt.(option (fun ppf -> pf ppf "@ of %a" pp_core_type))
        constructor_arg
    in
    Fmt.pf ppf " =@;<1 0>@[<v>%a@]" Fmt.(list ~sep:cut pp_constructor) constructors
  | Type_decl_record labels ->
    let pp_label ppf { Ast.label_name; label_arg } =
      Fmt.pf ppf "@[<hov 2>%a :@ %a@]" pp_label_name label_name.it pp_core_type label_arg
    in
    Fmt.pf ppf " =@;<1 0>@[<hov>{ %a }@]" Fmt.(list ~sep:(any ";@ ") pp_label) labels
;;

let pp_type_declaration keyword ppf ({ it; _ } : Ast.type_declaration) =
  let { Ast.type_decl_name; type_decl_params; type_decl_kind } = it in
  Fmt.pf
    ppf
    "@[<v 2>%s %a%a%a@]"
    keyword
    pp_type_params
    type_decl_params
    pp_type_name
    type_decl_name.it
    pp_type_decl_kind
    type_decl_kind
;;

let pp_type_declarations ppf = function
  | [] -> ()
  | declaration :: declarations ->
    Fmt.pf ppf "%a" (pp_type_declaration "type") declaration;
    List.iter declarations ~f:(Fmt.pf ppf "@,%a" (pp_type_declaration "and"))
;;

let pp_signature_item ~with_poly_params ppf ({ it; _ } : signature_item) =
  match it with
  | Sig_value bindings ->
    Fmt.(list ~sep:cut (pp_binding ~with_poly_params "val")) ppf bindings
  | Sig_primitive binding -> pp_binding ~with_poly_params "external" ppf binding
  | Sig_type declarations -> pp_type_declarations ppf declarations
;;

let pp ~with_poly_params ppf signature =
  Fmt.pf
    ppf
    "@[<v>%a@]"
    Fmt.(list ~sep:cut (pp_signature_item ~with_poly_params))
    signature
;;
