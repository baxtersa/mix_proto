open Ast
open Environment
open Smtlib

module type TYPECHECK =
sig
  val is_feasible : Symbolic_ast.sym_exp -> bool

  val typecheck : gamma -> exp -> typ
end

module type SYM =
sig
  type state

  val initial_state : state

  val guard_of : state -> Symbolic_ast.sym_exp
  val memory_of : state -> Symbolic_ast.sym_memory

  val sym_eval : Symbolic_environment.sigma -> state -> exp -> (state * Symbolic_ast.sym_exp) list
end

module type MAKE =
  functor (Sym:SYM) -> TYPECHECK

module Make : MAKE =
  functor (Sym:SYM) ->
  struct
    let type_of_const (c:const) : typ =
      match c with
      | Int _ -> TInt
      | Bool _ -> TBool

    let type_of_bop (op:binop) : typ =
      match op with
      | Add ->
        TFun (TInt, TFun (TInt, TInt))
      | Div ->
        TFun (TInt, TFun (TInt, TInt))
      | Eq ->
        TFun (TInt, TFun (TInt, TBool))
      | Conj ->
        TFun (TBool, TFun (TBool, TBool))
      | Disj ->
        TFun (TBool, TFun (TBool, TBool))

    let type_of_uop (op:unop) : typ =
      match op with
      | Neg ->
        TFun (TBool, TBool)

    let rec cmp_type (t:typ) (t':typ) : bool =
      match t, t' with
      | TInt, TInt
      | TBool, TBool ->
        true
      | TRef t, TRef t' ->
        cmp_type t t'
      | TFun (t, t'), TFun (t1, t2) ->
        cmp_type t t1 && cmp_type t' t2
      | _ ->
        false

    let z3 : solver = make_solver "./z3"

    let sort_of_typ (t:typ) : sort =
      match t with
      | TInt -> int_sort
      | TBool -> bool_sort
      | _ ->
        failwith "Translation of complex type to z3 sort not implemented."

    let rec declare_consts seen e : unit =
      let open Symbolic_ast in
      match e with
      | Typed (SymId x, TFun _) ->
        if List.mem x !seen
        then ()
        else
          begin
            seen := x :: !seen;
            declare_const z3 (Id x) (Sort (Id "fun"))
          end
      | Typed (SymId x, t) ->
        if List.mem x !seen
        then ()
        else
          begin
            seen := x :: !seen;
            declare_const z3 (Id x) (sort_of_typ t)
          end
      | Typed (e, t) ->
        declare_consts seen e
      | SymId _
      | SymConst _ ->
        ()
      | SymFun (x, t, _, _, _) ->
        seen := x :: !seen;
        declare_const z3 (Id x) (sort_of_typ t)
      | SymBinop (op, e1, e2) ->
        declare_consts seen e1;
        declare_consts seen e2
      | SymUnop (Neg, e) ->
        declare_consts seen e

    let rec build_z3_term (e:Symbolic_ast.sym_exp) : term =
      let open Symbolic_ast in
      match e with
      | Typed (e, t) ->
        build_z3_term e
      | SymId x ->
        Const (Id x)
      | SymConst (Int n) ->
        int_to_term n
      | SymConst (Bool b) ->
        bool_to_term b
      | SymFun (x, t, t', ctx, e) ->
        failwith "Don't know what to do here"
      | SymBinop (op, e1, e2) ->
        build_z3_term_of_op op e1 e2
      | SymUnop (Neg, e) ->
        not_ (build_z3_term e)
      | _ ->
        failwith "Translation of sym_e to z3 term not implemented"

    and build_z3_term_of_op op e1 e2 =
      match op with
      | Add ->
        add (build_z3_term e1) (build_z3_term e2)
      | Div ->
        App (Id "/", [build_z3_term e1; build_z3_term e2])
      | Eq ->
        equals (build_z3_term e1) (build_z3_term e2)
      | Disj ->
        or_ (build_z3_term e1) (build_z3_term e2)
      | Conj ->
        and_ (build_z3_term e1) (build_z3_term e2)

    let is_feasible  (guard:Symbolic_ast.sym_exp) : bool =
      push z3;
      declare_sort z3 (Id "fun") 0;
      declare_consts (ref []) guard;
      assert_ z3 (build_z3_term guard);
      match check_sat z3 with
      | Unsat ->
        pop z3;
        false
      | Sat ->
        pop z3;
        true
      | Unknown -> failwith "Solver failed to determine satisfiability of feasibility check."

    let is_tautology  (guard:Symbolic_ast.sym_exp) : bool =
      push z3;
      declare_sort z3 (Id "fun") 0;
      declare_consts (ref []) guard;
      (* Assert that the negation of the guard is unsat (i.e. the
         guard is a tautology. *)
      assert_ z3 (not_ (build_z3_term guard));
      match check_sat z3 with
      | Unsat ->
        pop z3;
        true
      | Sat ->
        pop z3;
        false
      | Unknown -> failwith "Solver failed to determine satisfiability of exhaustive check."

    let rec type_of_sym_results (results:Symbolic_ast.sym_exp list) : typ =
      match results with
      | [] -> failwith "No symbolic results to extract type from."
      | [Symbolic_ast.Typed (_, t)] ->
        t
      | Symbolic_ast.Typed (_, t) :: rest ->
        let t' = type_of_sym_results rest in
        if cmp_type t t'
        then t
        else failwith "Forked symbolic execution disagrees on the type of the symbolic result"
      | _ ->
        failwith "Symbolic execution returned to typechecker with untyped symbolic expression in forked results."

    let rec typecheck (env:gamma) (e:exp) : typ =
      match e with
      | Id x ->
        lookup_typ x env
      | Const c ->
        type_of_const c
      | Binop (op, e1, e2) ->
        (match type_of_bop op with
         | TFun (t, (TFun (t', t''))) ->
           let t1 = typecheck env e1 in
           let t2 = typecheck env e2 in
           if cmp_type t t1 && cmp_type t' t2
           then t''
           else failwith "Invalid type of operands."
         | _ ->
           failwith "Invalid type of operator.")
      | Unop (op, e) ->
        (match type_of_uop op with
         | TFun (t, t') ->
           let ty = typecheck env e in
           if cmp_type t ty
           then t'
           else failwith "Invalid type of operand."
         | _ ->
           failwith "Invalid type of operator.")
      | If (e1, e2, e3) ->
        (match typecheck env e1 with
         | TBool ->
           let t = typecheck env e2 in
           let t' = typecheck env e3 in
           if cmp_type t t'
           then t
           else failwith "Type of then/else branches must be the same."
         | _ ->
           failwith "Type of if condition must be bool.")
      | Let (x, e1, e2) ->
        let t = typecheck env e1 in
        typecheck ((x, t) :: env) e2
      | Ref e ->
        TRef (typecheck env e)
      | Assign (e1, e2) ->
        (match typecheck env e1 with
         | TRef t ->
           let t' = typecheck env e2 in
           if cmp_type t t'
           then TRef t
           else failwith "Can only assign an expression to a reference of the same type."
         | _ ->
           failwith "Can only assign to a reference.")
      | Deref e ->
        (match typecheck env e with
         | TRef t ->
           t
         | _ ->
           failwith "Can only deref a reference.")
      | Fun (x, t_dom, e) ->
        let t = typecheck ((x, t_dom) :: env) e in
        TFun (t_dom, t)
      (* | Fix (x, t, e) -> *)
      (*   let t' = typecheck ((x, t) :: env) e in *)
      (*   if cmp_type t t' *)
      (*   then t *)
      (*   else failwith ("Fix function expression is required to have type " ^ (show_typ t) ^ ".") *)
      | App (e1, e2) ->
        (match typecheck env e1 with
         | TFun (t, t') ->
           let targ = typecheck env e2 in
           if cmp_type t targ
           then t'
           else failwith "Type of argument does not match function domain type."
         | _ ->
           failwith "Type of expression in function position must be an arrow.")
      | TypedBlock e ->
        typecheck env e
      | SymbolicBlock e ->
        let sigma = generate_sym_env env in
        let s = Sym.initial_state in
        let results = Sym.sym_eval sigma s e in
        if List.length results = 0
        then failwith "Symbolic evaluation yielded no results.";
        let guard = List.fold_left (fun union (s', sym_e) ->
            Symbolic_ast.SymBinop (Disj, union, Sym.guard_of s'))
            (Symbolic_ast.SymConst (Bool true))
            results
        in
        match is_tautology guard with
        | true ->
          let _, types = List.split results in
          type_of_sym_results types
        | false ->
          failwith "Symbolic execution ws not exhaustive, and thus could yield unsound results."
  end
