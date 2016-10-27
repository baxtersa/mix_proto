open Ast
open Mix_parser
open Smtlib
open Symbolic_ast

module Command = Core.Std.Command

module rec T : Analyses.TYP = Typecheck.Make(SE)
and SE : Analyses.SYM = Symbolic_interp.Make(T)

open T
open SE

let symbolic (z3:solver) (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  print_endline (show_exp program);
  let results = sym_eval z3 [] initial_state program in
  let rec f results =
    match results with
    | [s, sym_e] ->
      let g = guard_of s in
      if is_feasible z3 g
      then begin
        print_endline "Output:";
        print_endline ("constraints:\t" ^ show_sym_exp g);
        print_endline ("symbolic val:\t" ^ show_sym_exp sym_e)
      end
      else ()
    | (s, sym_e) :: rest ->
      f [s, sym_e];
      f rest
    | _ -> failwith "Forked symbolic execution not yet implemented."
  in f results

let typed (z3:solver) (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  print_endline (show_exp program);
  let t = typecheck z3 [] program in
  print_endline "Output:";
  print_endline (show_typ t)

let run_prog z3 sym typ () =
  let z3 = make_solver z3 in
  match sym, typ with
  | Some s, None ->
     symbolic z3 s
  | None, Some s ->
     typed z3 s
  | Some _, Some _ ->
     failwith "Must specify only one of -sym or -typ to begin static analysis."
  | _ ->
     failwith "Please specify whether to begin static analysis under symbolic evaluator or type checker."

let spec =
  let open Command.Spec in
  empty
  +> flag "-z3" (optional_with_default "z3" string)
    ~doc:"FILE Path to z3 executable. Defaults to looking in system PATH"
  +> flag "-sym" (optional string)
    ~doc:"FILE Execute static analysis under symbolic evaluator on the given file."
  +> flag "-typ" (optional string)
    ~doc:"FILE Execute static analysis under type checker on the given file."

let command =
  Command.basic
    ~summary:"Prototype OCaml implementation of the hybrid symbolic execution/type checking system MIX."
    ~readme:(fun () -> "More detailed information")
    spec
    run_prog

let () =
  Command.run command
