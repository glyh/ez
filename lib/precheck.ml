open Ast
open File_path.Operators

let read_file_as_string filename =
  let ic = open_in ?/$filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let rec transform_precheck (our_module : File_path.t) (ast : prog0) :
    prog_precheck =
  let collect_definition : def0 -> def_precheck list = function
    | Function (ty, "main", params, body) ->
        [
          Function
            (ty, "main", params, Block [ Expr (Call ("ezstd_init", [])); body ]);
        ]
    | Function (ty, id, params, body) -> [ Function (ty, id, params, body) ]
    | Extern (ty, id, params) -> [ Extern (ty, id, params) ]
    | Require _module ->
        let start_path = File_path.dirname_exn our_module in
        let required_module_path = start_path /?/ ~/(_module ^ ".ez") in
        parse_and_transform_precheck required_module_path
  in
  ast |> List.map collect_definition |> List.concat

and parse_and_transform_precheck (path : File_path.t) : prog_precheck =
  Printf.printf "Reading %s\n" ?/$path;
  let source_code = read_file_as_string path in
  (* parse and type check *)
  Parser_nice.pp_exceptions ();

  let untyped_ast = Parser_nice.parse_string source_code in
  transform_precheck path untyped_ast
