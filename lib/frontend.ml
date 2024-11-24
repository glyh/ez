open Lexing
open Typecheck
open Ast
open Precheck
open Postcheck
open File_path.Operators

let colnum pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "line " ^ l ^ ", column " ^ c

let frontend source_path_string target_file =
  let precheck_ast = parse_and_transform_precheck ?/source_path_string in

  let typed_ast =
    try typecheck precheck_ast with
    | TypeMismatch (lhs, rhs) ->
        Printf.printf "Type mismatch: %s and %s" (type_to_string lhs)
          (type_to_string rhs);
        exit 1
    | BinTypeMismatch (op, lhs, rhs) ->
        Printf.printf "Type mismatch for %s: %s and %s" (binop_to_string op)
          (type_to_string lhs) (type_to_string rhs);
        exit 1
  in
  let transformed_ast = transform_postcheck typed_ast in

  (* serialize the AST *)
  let encoder = Pbrt.Encoder.create () in
  Ez_ir.encode_pb_program transformed_ast encoder;
  (* output the protobuf message to a file *)
  let oc = open_out target_file in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
