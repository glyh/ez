open Lexing
open Typecheck
open Ast

let colnum pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "line " ^ l ^ ", column " ^ c

let parse s = Parser_nice.parse_string s

let read_stdin () =
  let acc = ref "" in
  try
    while true do
      acc := !acc ^ "\n" ^ read_line ()
    done;
    ""
  with End_of_file -> !acc

let read_file_as_string filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let parse_and_check source_file target_file =
  let source_code = read_file_as_string source_file in
  (* parse and type check *)
  Parser_nice.pp_exceptions ();
  let untyped_ast = parse source_code in
  let typed_ast =
    try typecheck untyped_ast with
    | TypeMismatch (lhs, rhs) ->
        Printf.printf "Type mismatch: %s and %s" (type_to_string lhs)
          (type_to_string rhs);
        exit 1
    | BinTypeMismatch (op, lhs, rhs) ->
        Printf.printf "Type mismatch for %s: %s and %s" (binop_to_string op)
          (type_to_string lhs) (type_to_string rhs);
        exit 1
  in
  (* serialize the AST *)
  let encoder = Pbrt.Encoder.create () in
  Ez_ir.encode_pb_program typed_ast encoder;
  (* output the protobuf message to a file *)
  let oc = open_out target_file in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc

let main () =
  if Array.length Sys.argv < 3 then
    print_endline "USAGE: ez [SOURCE.ez] [TARGET.irpb]"
  else
    let source_file = Sys.argv.(1) in
    let target_file = Sys.argv.(2) in
    Printf.printf "Input: %s\nOutput: %s\n" source_file target_file;
    parse_and_check source_file target_file
