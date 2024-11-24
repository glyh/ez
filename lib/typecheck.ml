open Ez_ir
open Ast

exception TypeMismatch of ez_type * ez_type
exception BinTypeMismatch of expr_binary_bin_op * ez_type * ez_type
exception CallMismatch of identifier * ez_type list * ez_type list
exception DerefernencingNonPointer of ez_type

type function_type = ez_type list * ez_type

let rec type_to_string (t : ez_type) : string =
  match t with
  | Non_ptr Unit -> "()"
  | Non_ptr I64 -> "i64"
  | Non_ptr Str -> "string"
  | Non_ptr F64 -> "f64"
  | Non_ptr Bool -> "bool"
  | Ptr inner -> "ptr[" ^ type_to_string inner ^ "]"

let dump_type_chain (params_ty : ez_type list) : string =
  match params_ty with
  | [] -> "()"
  | _ -> params_ty |> List.map type_to_string |> String.concat "->"

let dump_funtion_type (ty : function_type) : string =
  let params_ty, return_ty = ty in
  dump_type_chain params_ty ^ "->" ^ type_to_string return_ty

let dump_function_sig (fsig : identifier * function_type) : string =
  let fname, ty = fsig in
  fname ^ ": " ^ dump_funtion_type ty

let definiton_to_function_type (def : def_precheck) :
    (string * (ez_type list * ez_type)) option =
  match def with
  | Function (return_type, name, args, _) | Extern (return_type, name, args) ->
      Some
        ( name,
          (args |> List.map (fun { param_type; _ } -> param_type), return_type)
        )

module StrMap = Map.Make (String)

type funcsig_map = function_type StrMap.t
type local_map = ez_type StrMap.t

let dump_function_sigs (sigs : funcsig_map) : string =
  let sigs_list = sigs |> StrMap.to_list in
  List.map dump_function_sig sigs_list |> String.concat "\n"

let collect_function_signatures (p : prog_precheck) : funcsig_map =
  p
  |> List.map definiton_to_function_type
  |> List.filter_map (fun x -> x)
  |> StrMap.of_list

let rec check_expression (sigs : funcsig_map) (locals : local_map) (e : expr0) :
    expr =
  match e with
  | BinOp (((Add | Sub | Mul | Div | Mod) as op), lhs, rhs) -> (
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = typed_rhs.expr_type in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = typed_lhs.expr_type in
      match (lhs_type, rhs_type) with
      | Non_ptr I64, Non_ptr I64 ->
          {
            expr_type = Non_ptr I64;
            kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
          }
      | Non_ptr F64, Non_ptr F64 ->
          {
            expr_type = Non_ptr F64;
            kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
          }
      | _ -> raise (BinTypeMismatch (op, lhs_type, rhs_type)))
  | BinOp (((Eq | Noteq) as op), lhs, rhs) ->
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = typed_rhs.expr_type in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = typed_lhs.expr_type in
      if 0 == compare lhs_type rhs_type then
        {
          expr_type = Non_ptr Bool;
          kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
        }
      else raise (BinTypeMismatch (op, lhs_type, rhs_type))
  | BinOp (((Lessthan | Lesseq | Greaterthan | Greatereq) as op), lhs, rhs) -> (
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = typed_rhs.expr_type in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = typed_lhs.expr_type in
      match (lhs_type, rhs_type) with
      | Non_ptr I64, Non_ptr I64 ->
          {
            expr_type = Non_ptr Bool;
            kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
          }
      | Non_ptr F64, Non_ptr F64 ->
          {
            expr_type = Non_ptr Bool;
            kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
          }
      | _ -> raise (BinTypeMismatch (op, lhs_type, rhs_type)))
  | BinOp (((Land | Lor) as op), lhs, rhs) -> (
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = typed_rhs.expr_type in
      let typed_lhs = check_expression sigs locals lhs in
      let lhs_type = typed_lhs.expr_type in
      match (lhs_type, rhs_type) with
      | Non_ptr Bool, Non_ptr Bool ->
          {
            expr_type = Non_ptr Bool;
            kind = Binary { op; lhs = typed_lhs; rhs = typed_rhs };
          }
      | _ -> raise (BinTypeMismatch (op, lhs_type, rhs_type)))
  | Val (I64_val i) -> { expr_type = Non_ptr I64; kind = Value (I64_val i) }
  | Val (Str_val s) -> { expr_type = Non_ptr Str; kind = Value (Str_val s) }
  | Val (F64_val f) -> { expr_type = Non_ptr F64; kind = Value (F64_val f) }
  | Val (Bool_val b) -> { expr_type = Non_ptr Bool; kind = Value (Bool_val b) }
  | Val Unit_val -> { expr_type = Non_ptr Unit; kind = Value Unit_val }
  | Call (callee, exps) ->
      let exps_checked =
        List.map (fun e -> check_expression sigs locals e) exps
      in
      let type_of_exps = List.map (fun exp -> exp.expr_type) exps_checked in
      let param_types, ret_type = StrMap.find callee sigs in
      if 0 == compare type_of_exps param_types then
        { expr_type = ret_type; kind = Call { callee; args = exps_checked } }
      else raise (CallMismatch (callee, param_types, type_of_exps))
  | Var id ->
      let expr_type = StrMap.find id locals in
      { kind = Variable id; expr_type }

let rec check_statement (return_ty : ez_type) (sigs : funcsig_map)
    (locals : local_map) (stmt : stmt0) : local_map * statement =
  match stmt with
  | Assign (name, rhs) ->
      let typed_rhs = check_expression sigs locals rhs in
      let rhs_type = typed_rhs.expr_type in
      let var_type = StrMap.find name locals in
      if 0 != compare rhs_type var_type then
        raise (TypeMismatch (rhs_type, var_type))
      else (locals, Assign { name; rhs = typed_rhs })
  | Expr exp -> (locals, Expr (check_expression sigs locals exp))
  | Declaration (decl_type, name, exp) ->
      let typed_rhs = check_expression sigs locals exp in
      let rhs_type = typed_rhs.expr_type in
      if 0 != compare rhs_type decl_type then
        raise (TypeMismatch (rhs_type, decl_type))
      else
        ( StrMap.add name decl_type locals,
          Declaration { name; rhs = typed_rhs; type_ = rhs_type } )
  | If (cond, then_clause, else_clause) ->
      let typed_cond = check_expression sigs locals cond in
      let _, then_clause_typed =
        check_statement return_ty sigs locals then_clause
      in
      let _, else_clause_typed =
        check_statement return_ty sigs locals else_clause
      in
      ( locals,
        If
          {
            condition = typed_cond;
            then_branch = then_clause_typed;
            else_branch = else_clause_typed;
          } )
  | Block stmts -> (
      match check_statements return_ty sigs locals stmts with
      | locals, stmts -> (locals, Block { statements = stmts }))
  | While (cond, body) ->
      let cond_checked = check_expression sigs locals cond in
      let _, body_checked = check_statement return_ty sigs locals body in
      (locals, While { condition = cond_checked; body = body_checked })
  | Return exp ->
      let typed_return_value = check_expression sigs locals exp in
      let return_type_inferred = typed_return_value.expr_type in
      if 0 != compare return_type_inferred return_ty then
        raise (TypeMismatch (return_type_inferred, return_ty))
      else (locals, Return typed_return_value)

and check_statements (return_ty : ez_type) (sigs : funcsig_map)
    (locals : local_map) (stmts : stmt0 list) : local_map * statement list =
  match stmts with
  | [] -> (locals, [])
  | stmt :: rest_stmts ->
      let locals_new, stmt_checked =
        check_statement return_ty sigs locals stmt
      in
      let locals_final, rest_checked =
        check_statements return_ty sigs locals_new rest_stmts
      in
      (locals_final, stmt_checked :: rest_checked)

let check_definitons (sigs : funcsig_map) (def_precheck_to_check : def_precheck)
    : definition list =
  match def_precheck_to_check with
  | Extern (return_type, name, params) ->
      [ Extern { return_type; name; params } ]
  | Function (return_type, name, params, body) ->
      let initial_params =
        params
        |> List.map (fun { param_type; name } -> (name, param_type))
        |> StrMap.of_list
      in
      let _, statement_checked =
        check_statement return_type sigs initial_params body
      in
      [ Func { return_type; name; params; body = statement_checked } ]

let typecheck (p : prog_precheck) : program =
  let function_sigs = collect_function_signatures p in
  let definitions =
    p |> List.map (check_definitons function_sigs) |> List.concat
  in
  { definitions }
