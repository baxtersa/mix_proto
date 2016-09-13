open Ast
open Parser

let _ =
  let filename : string = Sys.argv.(1) in
  let program : exp = from_file filename in
  failwith "MIX not implemented..."
