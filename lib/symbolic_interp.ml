open Ast
open Symbolic_ast
open Symbolic_environment

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

let count = ref 0
let fresh_sym () : sym_id =
  incr count;
  "_a" ^ (string_of_int !count)

let rec sym_eval (ctx:sigma) (s:state) (e:exp) : state * sym_exp =
  match e with
  | Id x ->
    s, lookup_exp x ctx
  | Const c ->
    s, Typed (SymConst c, typeof e)
  | Binop (op, e1, e2) ->
    sym_eval_binop ctx s op e1 e2
  | Unop (op, e) ->
    sym_eval_unop ctx s op e
  | If (e1, e2, e3) ->
    let s1, g = sym_eval ctx s e1 in
    let s1' = with_guard s1 (SymBinop (Conj, guard_of s1, g)) in
    let s2, sym_e2 = sym_eval ctx s1' e2 in
    let s1'' = with_guard s1 (SymBinop (Conj, guard_of s1, SymUnop (Neg, g))) in
    let s3, sym_e3 = sym_eval ctx s1'' e3 in
    s, SymId "Crazyness"
  | Let (x, e1, e2) ->
    let s1, sym_e1 = sym_eval ctx s e1 in
    sym_eval ((x, sym_e1) :: ctx) s1 e2
  | Ref e ->
     let s1, sym_e = sym_eval ctx s e in
     (match sym_e with
      | Typed (sym_e', t) ->
         let alpha = fresh_sym () in
         let sym_alpha = Typed (SymId alpha, TRef t) in
         let s' = with_memory s1 (Alloc (memory_of s1, sym_alpha, sym_e)) in
         s', sym_alpha
      | _ ->
         failwith "Unexpected symbolic expression evaluating reference.")
  | Assign (e1, e2) ->
     let s1, sym_e1 = sym_eval ctx s e1 in
     let s2, sym_e2 = sym_eval ctx s1 e2 in
     let s' = with_memory s2 (Update (memory_of s2, sym_e1, sym_e2)) in
     s', sym_e2
  | SymbolicBlock e ->
     sym_eval ctx s e

and sym_eval_binop (ctx:sigma) (s:state)
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

and sym_eval_unop (ctx:sigma) (s:state) (op:unop) (e:exp) : state * sym_exp =
  match op with
  | Neg ->
    let s', u = sym_eval ctx s e in
    (match u with
     | Typed (_, TBool) ->
       s, Typed (SymUnop (op, u), TBool)
     | _ ->
       failwith ("Negation expected operand of type bool."))
