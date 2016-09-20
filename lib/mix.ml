open Ast
open Parser

let _ =
  let filename : string = Sys.argv.(1) in
  let program : exp = from_file filename in
  print_endline "Input:";
  pp_exp Format.std_formatter program;
  print_newline ();
  failwith "MIX not implemented..."
