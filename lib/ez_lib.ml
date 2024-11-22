open Frontend

let main () =
  if Array.length Sys.argv < 3 then
    print_endline "USAGE: ez [SOURCE.ez] [TARGET.irpb]"
  else
    let source_file = Sys.argv.(1) in
    let target_file = Sys.argv.(2) in
    Printf.printf "Input: %s\nOutput: %s\n" source_file target_file;
    parse_and_check source_file target_file
