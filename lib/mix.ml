open Ast
open Mix_parser
open Symbolic_ast

module Command = Core.Std.Command

module rec T : Analyses.TYP = Typecheck.Make(SE)
and SE : Analyses.SYM = Symbolic_interp.Make(T)

open T
open SE

let symbolic (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  print_endline (show_exp program);
  let results = sym_eval [] initial_state program in
  let rec f results =
    match results with
    | [s, sym_e] ->
      let g = guard_of s in
      if is_feasible g
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

let typed (s:string) =
  let filename : string = s in
  let program : exp = from_file filename in
  print_endline "Input:";
  print_endline (show_exp program);
  let t = typecheck [] program in
  print_endline "Output:";
  print_endline (show_typ t)

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
