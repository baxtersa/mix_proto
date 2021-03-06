open Ast
open Smtlib
open Symbolic_ast
open Symbolic_environment

module type SYM = sig
  type state

  val initial_state : state

  val guard_of : state -> sym_exp
  val memory_of : state -> sym_memory

  val sym_eval : solver -> sigma -> state -> Ast.exp -> (state * sym_exp) list
end

module type TYP = sig
  val is_feasible : solver -> sym_exp -> bool

  val typecheck : solver -> Environment.gamma -> Ast.exp -> Ast.typ
end

module type MAKE =
  functor (Typecheck:TYP) -> SYM

module Make : MAKE =
  functor (Typecheck:TYP) ->
  struct
    (* A symbolic state is a tuple (g, m), where 'g' is a symbolic expression
       constrained to be a path condition and m a symbolic memory. *)
    type state = sym_exp * sym_memory

    let initial_state : state = (SymConst (Bool true), Arbitrary)

    let typeof (e:exp) : typ =
      match e with
      | Const (Int _) ->
        TInt
      | Const (Bool _) ->
        TBool
      | _ ->
        failwith "Expected a value."

    let with_guard ((_, m):state) (e:sym_exp) : state =
      e, m
    let guard_of ((g, _):state) : sym_exp =
      g

    let with_memory ((e, _):state) (m:sym_memory) : state =
      e, m
    let memory_of ((_, m):state) : sym_memory =
      m

    let rec sym_eval (z3:solver) (ctx:sigma) (s:state) (e:exp) : (state * sym_exp) list =
      if not (Typecheck.is_feasible z3 (guard_of s))
      then []
      else
        match e with
        | Id x ->
          [s, lookup_exp x ctx]
        | Const c ->
          [s, Typed (SymConst c, typeof e)]
        | Binop (op, e1, e2) ->
          sym_eval_binop z3 ctx s op e1 e2
        | Unop (op, e) ->
          sym_eval_unop z3 ctx s op e
        | If (e1, e2, e3) ->
          let results_guard = sym_eval z3 ctx s e1 in
          let results_then = List.map (fun (s1, g) ->
              let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, g)) in
              if Typecheck.is_feasible z3 (guard_of s1')
              then sym_eval z3 ctx s1' e2
              else [])
              results_guard
                             |> List.concat in
          let results_else = List.map (fun (s1, g) ->
              let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, SymUnop (Neg, g))) in
              if Typecheck.is_feasible z3 (guard_of s1')
              then sym_eval z3 ctx s1' e3
              else [])
              results_guard
                             |> List.concat in
          results_then @ results_else
        | Let (x, e1, e2) ->
          let results = sym_eval z3 ctx s e1 in
          List.map (fun (s1, sym_e1) ->
              sym_eval z3 ((x, sym_e1) :: ctx) s1 e2)
            results
          |> List.concat
        (* | Ref e -> *)
        (*   let results = sym_eval z3 ctx s e in *)
        (*   List.map (fun (s1, sym_e) -> *)
        (*       match sym_e with *)
        (*       | Typed (sym_e', t) -> *)
        (*         let alpha = fresh_sym () in *)
        (*         let sym_alpha = Typed (SymId alpha, TRef t) in *)
        (*         let s' = with_memory s1 (Alloc (memory_of s1, sym_alpha, sym_e)) in *)
        (*         s', sym_alpha *)
        (*       | _ -> *)
        (*         failwith "Unexpected symbolic expression evaluating reference.") *)
        (*     results *)
        (* | Assign (e1, e2) -> *)
        (*   let results1 = sym_eval z3 ctx s e1 in *)
        (*   List.map (fun (s1, sym_e1) -> *)
        (*       let results2 = sym_eval z3 ctx s1 e2 in *)
        (*       List.map (fun (s2, sym_e2) -> *)
        (*           let s' = with_memory s2 (Update (memory_of s2, sym_e1, sym_e2)) in *)
        (*           s', sym_e2) *)
        (*         results2) *)
        (*     results1 *)
        (*   |> List.concat *)
        | Fun (x, t_dom, e) ->
          let sym_es = sym_eval z3 ((x, Typed (SymId x, t_dom)) :: ctx) s e in
          List.map (fun (s, sym_e) ->
              match sym_e with
              | Typed (sym_e, t) ->
                s, Typed (SymFun (x, t_dom, t, ctx, e), TFun (t_dom, t))
              | _ ->
                failwith "Symbolic evaluation should only yield typed symbolic expressions.")
            sym_es
        (* [s, Typed (SymFun (x, t_dom, t_cod, ctx, e), TFun (t_dom, t_cod))] *)
        (* | Fix (x, t, e) -> *)
        | App (f, arg) ->
          let results_fun = sym_eval z3 ctx s f in
          List.map (fun (s_f, sym_f) ->
              let results_arg = sym_eval z3 ctx s_f arg in
              List.map (fun (s_arg, sym_arg) ->
                  match sym_f with
                  | Typed (SymFun (x, t, _, ctx', e), _)
                  | SymFun (x, t, _, ctx', e) ->
                    let s_app = with_guard s_arg (SymBinop (Conj, guard_of s_arg, SymBinop (Eq, Typed (SymId x, t), sym_arg))) in
                    sym_eval z3 ((x, sym_arg) :: ctx') s_app e
                  | Typed (SymId x, TFun (t, t')) ->
                    let sym_id = fresh_sym () in
                    [s, Typed (SymId sym_id, t')]
                  | _ ->
                    failwith "Symbolic expression in function position must be a symbolic function.")
                results_arg
              |> List.concat)
            results_fun
          |> List.concat
        | TypedBlock e ->
          (try
             let gamma = generate_type_env ctx in
             let t = Typecheck.typecheck z3 gamma e in
             let alpha = fresh_sym () in
             let s' = with_memory s (Arbitrary) in
             [s', Typed (SymId alpha, t)]
           with Failure err ->
             (match sym_eval z3 ctx s e with
              | [] -> []
              | _ ->
                failwith err))
        | SymbolicBlock e ->
          sym_eval z3 ctx s e

    and sym_eval_binop (z3:solver) (ctx:sigma) (s:state)
        (op:binop) (e1:exp) (e2:exp) : (state * sym_exp) list =
      match op with
      | Add ->
        let results1 = sym_eval z3 ctx s e1 in
        List.map (fun (s1, u1) ->
            let results2 = sym_eval z3 ctx s1 e2 in
            List.map (fun (s2, u2) ->
                match u1, u2 with
                | Typed (_, TInt), Typed (_, TInt) ->
                  s2, Typed (SymBinop (op, u1, u2), TInt)
                | _ ->
                  failwith ("Addition expected operands of type int."))
              results2)
          results1
        |> List.concat
      | Div ->
        let results1 = sym_eval z3 ctx s e1 in
        List.map (fun (s1, u1) ->
            let results2 = sym_eval z3 ctx s1 e2 in
            List.map (fun (s2, u2) ->
                match u1, u2 with
                | Typed (_, TInt), Typed (_, TInt) ->
                  s2, Typed (SymBinop (op, u1, u2), TInt)
                | _ ->
                  failwith ("Division expected operands of type int."))
              results2)
          results1
        |> List.concat
      | Eq ->
        let results1 = sym_eval z3 ctx s e1 in
        List.map (fun (s1, u1) ->
            let results2 = sym_eval z3 ctx s1 e2 in
            List.map (fun (s2, u2) ->
                match u1, u2 with
                | Typed (_, TInt), Typed (_, TInt) ->
                  s2, Typed (SymBinop (op, u1, u2), TInt)
                | _ ->
                  failwith ("Equality expected operands of type int."))
              results2)
          results1
        |> List.concat
      | Conj ->
        let results1 = sym_eval z3 ctx s e1 in
        List.map (fun (s1, u1) ->
            let results2 = sym_eval z3 ctx s1 e2 in
            List.map (fun (s2, u2) ->
                match u1, u2 with
                | Typed (_, TBool), Typed (_, TBool) ->
                  s2, Typed (SymBinop (op, u1, u2), TBool)
                | _ ->
                  failwith ("Conjunction expected operands of type bool."))
              results2)
          results1
        |> List.concat
      | Disj ->
        let results1 = sym_eval z3 ctx s e1 in
        List.map (fun (s1, u1) ->
            let results2 = sym_eval z3 ctx s1 e2 in
            List.map (fun (s2, u2) ->
                match u1, u2 with
                | Typed (_, TBool), Typed (_, TBool) ->
                  s2, Typed (SymBinop (op, u1, u2), TBool)
                | _ ->
                  failwith ("Disjunction expected operands of type bool."))
              results2)
          results1
        |> List.concat

    and sym_eval_unop (z3:solver) (ctx:sigma) (s:state) (op:unop) (e:exp) : (state * sym_exp) list =
      match op with
      | Neg ->
        let results = sym_eval z3 ctx s e in
        List.map (fun (s', u) ->
            (match u with
             | Typed (_, TBool) ->
               [s, Typed (SymUnop (op, u), TBool)]
             | _ ->
               failwith ("Negation expected operand of type bool.")))
          results
        |> List.concat
  end
