open Ast
open Parser
open Symbolic_ast

module Command = Core.Std.Command

module rec T : Symbolic_interp.TYP = Typecheck.Make(SE)
and SE : Typecheck.SYM = Symbolic_interp.Make(T)

open T
open SE

let symbolic (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  pp_exp Format.std_formatter program;
  print_newline ();
  let s, _ = sym_eval [] initial_state program in
  let g = guard_of s in
  print_endline "Output:";
  pp_sym_exp Format.std_formatter g;
  print_newline ()

let typed (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  pp_exp Format.std_formatter program;
  print_newline ();
  let t = typecheck [] program in
  print_endline "Output:";
  pp_typ Format.std_formatter t;
  print_newline ()

let run_prog sym typ () =
  match sym, typ with
  | Some s, None ->
     symbolic s
  | None, Some s ->
     typed s
  | Some _, Some _ ->
     failwith "Must specify only one of -sym or -typ to begin static analysis."
  | _ ->
     failwith "Please specify whether to begin static analysis under symbolic evaluator or type checker."

let spec =
  let open Command.Spec in
  empty
  +> flag "-sym" (optional string)
    ~doc:"FILE Execute static analysis under symbolic evaluator on the given file."
  +> flag "-typ" (optional string)
    ~doc:"FILE Execute static analysis under type checker on the given file."

let command =
  Command.basic
    ~summary:"Launch Supernet virtual compiler and SDN controller"
    ~readme:(fun () -> "More detailed information")
    spec
    run_prog

let () =
  Command.run command
