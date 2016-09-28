open Ast
open Symbolic_ast
open Symbolic_environment

(* A symbolic state is a tuple (g, m), where 'g' is a symbolic expression
   constrained to be a path condition and m a symbolic memory. *)
type state = sym_exp * sym_memory

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

let rec sym_eval (ctx:sigma ref) (s:state) (e:exp) : state * sym_exp =
  match e with
  | Id x ->
    s, (try lookup_exp x !ctx
        with Not_found -> failwith ("Unknown identifier:\t" ^ x))
  | Const c ->
    s, Typed (SymConst c, typeof e)
  | Binop (op, e1, e2) ->
    sym_eval_binop ctx s op e1 e2
  | Unop (op, e) ->
    sym_eval_unop ctx s op e
  | Let (x, e1, e2) ->
    let s1, sym_e1 = sym_eval ctx s e1 in
    extend x sym_e1 ctx;
    sym_eval ctx s1 e2
  | If (e1, e2, e3) ->
    let s1, g = sym_eval ctx s e1 in
    let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, g)) in
    sym_eval ctx s1' e2
  (* | If (e1, e2, e3) -> *)
  (*   let s1, g = sym_eval ctx s e1 in *)
  (*   let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, SymUnop (Neg g))) in *)
  (*   sym_eval ctx s1' e2 *)

and sym_eval_binop (ctx:sigma ref) (s:state)
    (op:binop) (e1:exp) (e2:exp) : state * sym_exp =
  match op with
  | Add ->
    let s1, u1 = sym_eval ctx s e1 in
    let s2, u2 = sym_eval ctx s1 e2 in
    (match u1, u2 with
     | Typed (_, TInt), Typed (_, TInt) ->
       s2, Typed (SymBinop (op, u1, u2), TInt)
     | _ ->
       failwith ("Addition expected operands of type int."))
  | Eq ->
    let s1, u1 = sym_eval ctx s e1 in
    let s2, u2 = sym_eval ctx s1 e2 in
    (match u1, u2 with
     | Typed (_, TInt), Typed (_, TInt) ->
       s2, Typed (SymBinop (op, u1, u2), TInt)
     | _ ->
       failwith ("Equality expected operands of type int."))
  | Conj ->
    let s1, u1 = sym_eval ctx s e1 in
    let s2, u2 = sym_eval ctx s1 e2 in
    (match u1, u2 with
     | Typed (_, TBool), Typed (_, TBool) ->
       s2, Typed (SymBinop (op, u1, u2), TBool)
     | _ ->
       failwith ("Conjunction expected operands of type bool."))

and sym_eval_unop (ctx:sigma ref) (s:state) (op:unop) (e:exp) : state * sym_exp =
  match op with
  | Neg ->
    let s', u = sym_eval ctx s e in
    (match u with
     | Typed (_, TBool) ->
       s, Typed (SymUnop (op, u), TBool)
     | _ ->
       failwith ("Negation expected operand of type bool."))
