open Ast
open Environment

let type_of_const (c:const) : typ =
  match c with
  | Int _ -> TInt
  | Bool _ -> TBool

let type_of_bop (op:binop) : typ =
  match op with
  | Add ->
     TFun (TInt, TFun (TInt, TInt))
  | Eq ->
     TFun (TInt, TFun (TInt, TBool))
  | Conj ->
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
  | Fun (x, t, e) ->
     let t' = typecheck ((x, t) :: env) e in
     TFun (t, t')
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
    let s = Symbolic_interp.initial_state in
    typecheck env e
