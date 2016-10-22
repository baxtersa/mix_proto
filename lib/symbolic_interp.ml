open Ast
open Symbolic_ast
open Symbolic_environment

module type SYM = sig
  type state

  val initial_state : state

  val guard_of : state -> sym_exp
  val memory_of : state -> sym_memory

  val sym_eval : sigma -> state -> Ast.exp -> (state * sym_exp) list
end

module type TYP = sig
  val typecheck : Environment.gamma -> Ast.exp -> Ast.typ
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

    let rec sym_eval (ctx:sigma) (s:state) (e:exp) : (state * sym_exp) list =
      let rec sym_eval' (ctx:sigma) (s:state) (e:exp) : state * sym_exp =
        match e with
        | Id x ->
          s, lookup_exp x ctx
        | Const c ->
          s, Typed (SymConst c, typeof e)
        | Binop (op, e1, e2) ->
          sym_eval'_binop ctx s op e1 e2
        | Unop (op, e) ->
          sym_eval'_unop ctx s op e
        | If (e1, e2, e3) ->
          let s1, g = sym_eval' ctx s e1 in
          let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, g)) in
          let s2, sym_e2 = sym_eval' ctx s1' e2 in
          let s1'' = with_guard s1 (SymBinop (Conj, guard_of s1, SymUnop (Neg, g))) in
          let s3, sym_e3 = sym_eval' ctx s1'' e3 in
          s, SymId "Crazyness"
        | Let (x, e1, e2) ->
          let s1, sym_e1 = sym_eval' ctx s e1 in
          sym_eval' ((x, sym_e1) :: ctx) s1 e2
        | Ref e ->
          let s1, sym_e = sym_eval' ctx s e in
          (match sym_e with
           | Typed (sym_e', t) ->
             let alpha = fresh_sym () in
             let sym_alpha = Typed (SymId alpha, TRef t) in
             let s' = with_memory s1 (Alloc (memory_of s1, sym_alpha, sym_e)) in
             s', sym_alpha
           | _ ->
             failwith "Unexpected symbolic expression evaluating reference.")
        | Assign (e1, e2) ->
          let s1, sym_e1 = sym_eval' ctx s e1 in
          let s2, sym_e2 = sym_eval' ctx s1 e2 in
          let s' = with_memory s2 (Update (memory_of s2, sym_e1, sym_e2)) in
          s', sym_e2
        | Fun (x, t, e) ->
          let gamma = generate_type_env ctx in
          let t' = Typecheck.typecheck ((x, t) :: gamma) e in
          s, Typed (SymFun (x, t, e), TFun (t, t'))
        (* | Fix (x, t, e) -> *)
        | App (f, arg) ->
          let s_f, sym_f = sym_eval' ctx s f in
          let s_arg, sym_arg = sym_eval' ctx s arg in
          (match sym_f with
           | Typed (SymFun (x, t, e), _)
           | SymFun (x, t, e) ->
             sym_eval' ((x, sym_arg) :: ctx) s e
           | _ ->
             failwith "Symbolic expression in function position must be a symbolic function.")
        | TypedBlock e ->
          let gamma = generate_type_env ctx in
          let t = Typecheck.typecheck gamma e in
          let alpha = fresh_sym () in
          let s' = with_memory s (Arbitrary) in
          s', Typed (SymId alpha, t)
        | SymbolicBlock e ->
          sym_eval' ctx s e

      and sym_eval'_binop (ctx:sigma) (s:state)
          (op:binop) (e1:exp) (e2:exp) : state * sym_exp =
        match op with
        | Add ->
          let s1, u1 = sym_eval' ctx s e1 in
          let s2, u2 = sym_eval' ctx s1 e2 in
          (match u1, u2 with
           | Typed (_, TInt), Typed (_, TInt) ->
             s2, Typed (SymBinop (op, u1, u2), TInt)
           | _ ->
             failwith ("Addition expected operands of type int."))
        | Eq ->
          let s1, u1 = sym_eval' ctx s e1 in
          let s2, u2 = sym_eval' ctx s1 e2 in
          (match u1, u2 with
           | Typed (_, TInt), Typed (_, TInt) ->
             s2, Typed (SymBinop (op, u1, u2), TInt)
           | _ ->
             failwith ("Equality expected operands of type int."))
        | Conj ->
          let s1, u1 = sym_eval' ctx s e1 in
          let s2, u2 = sym_eval' ctx s1 e2 in
          (match u1, u2 with
           | Typed (_, TBool), Typed (_, TBool) ->
             s2, Typed (SymBinop (op, u1, u2), TBool)
           | _ ->
             failwith ("Conjunction expected operands of type bool."))
        | Disj ->
          let s1, u1 = sym_eval' ctx s e1 in
          let s2, u2 = sym_eval' ctx s1 e2 in
          (match u1, u2 with
           | Typed (_, TBool), Typed (_, TBool) ->
             s2, Typed (SymBinop (op, u1, u2), TBool)
           | _ ->
             failwith ("Disjunction expected operands of type bool."))

      and sym_eval'_unop (ctx:sigma) (s:state) (op:unop) (e:exp) : state * sym_exp =
        match op with
        | Neg ->
          let s', u = sym_eval' ctx s e in
          (match u with
           | Typed (_, TBool) ->
             s, Typed (SymUnop (op, u), TBool)
           | _ ->
             failwith ("Negation expected operand of type bool."))
      in
      [sym_eval' ctx s e]
  end
