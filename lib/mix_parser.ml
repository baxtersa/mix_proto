open Ast
open MParser
open MParser_RE
open Tokens


type 'a parser = ('a, unit) MParser.t

let app_pattern p mk_app =
  let rec f e1 s = (
    (p >>= fun e1' -> f (mk_app e1 e1')) <|>
    (return e1)
  ) s in
  p >>= f

let keywords = [
  "true"; "false"; "empty"; "head"; "tail"; "empty?";
  "if"; "then"; "else"; "let"; "in"; "fun"; "fix"; "ref";
  "MIX(typed)"; "MIX(symbolic)"
]

let id : string parser =
  attempt (
    regexp (make_regexp "[A-Za-z_][A-Za-z_0-9_']*") >>= fun str ->
    if List.mem str keywords then fail "reserved word" else return str)
  <<< spaces                    (* >> *)

let rev_fold_left f xs = match List.rev xs with
  | [] -> raise (Failure "expected at least one element (internal error)")
  | x :: xs -> List.fold_left f x xs

let infix sym op : (exp, unit) operator =
  let f e1 e2 = Binop (op, e1, e2) in
  Infix (skip_symbol sym >> return f, Assoc_left)

let prefix sym op : (exp, unit) operator =
  let f e = Unop (op, e) in
  Prefix (skip_symbol sym >> return f)

let operators : (exp, unit) operator list list = [
  [infix "+" Add; infix "/" Div];
  [infix "==" Eq; prefix "~" Neg;
   infix "&&" Conj; infix "||" Disj];
]

let rec typ_atom s = (
  parens typ <|>
  (symbol "int" |>> fun _ -> TInt) <|>
  (symbol "bool" |>> fun _ -> TBool)
) s

(* and typ_ref s = ( *)
(*   typ_atom >>= fun t -> ( *)
(*     (skip_symbol "ref" |>> fun _ -> TRef t) <|> *)
(*     (return t)) *)
(* ) s *)

and typ s = (
  typ_atom >>= fun t1 -> (
    (symbol "->" >> typ |>> fun t2 -> TFun (t1, t2)) <|>
    (return t1))
) s

let rec atoms s = (
  parens exp <|>
  (between
     (symbol "MIX(typed)" >> spaces >> symbol "{")
     (symbol "}") exp |>>
   fun e -> TypedBlock e) <|>
  (between
     (symbol "MIX(symbolic)" >> spaces >> symbol "{")
     (symbol "}") exp |>>
   fun e -> SymbolicBlock e) <|>
  (symbol "true" |>> fun _ -> Const (Bool true)) <|>
  (symbol "false" |>> fun _ -> Const (Bool false)) <|>
  (decimal |>> (fun n -> Const (Int n))) <|>
  (id |>> (fun x -> Id x))
) s

(* and refs' e s = ( *)
(*   (symbol ":=" >> exp |>> fun e' -> Assign (e, e')) <|> *)
(*   (return e) *)
(* ) s *)

(* and refs s = ( *)
(*   (symbol "ref" >> exp |>> fun e -> Ref e) <|> *)
(*   (symbol "!" >> atoms |>> fun x -> Deref x) <|> *)
(*   (atoms >>= fun e -> refs' e) *)
(* ) s *)

and app' e s = (
  (atoms >>= fun e' -> app' (App (e, e'))) <|>
  (return e)
) s

and app s = (
  (atoms >>= fun e -> app' e)
) s

and cmp s = expression operators app s

and exp s = (
  cmp <|>
  (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
     (fun e1 e2 e3 -> If (e1, e2, e3))) <|>
  (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
     (fun x e1 e2 -> Let (x, e1, e2))) <|>
  (pipe3 (symbol "fun" >> symbol "(" >> id) (symbol ":" >> typ) (symbol ")" >> symbol "->" >> exp)
     (fun x t e -> Fun (x, t, e)))
  (* (pipe3 (symbol "fix" >> symbol "(" >> id) *)
  (*    (symbol ":" >> typ) *)
  (*    (symbol ")" >> symbol "->" >> exp) *)
  (*    (fun x t e -> Fix (x, t, e))) *)
) s

let from_string (str : string) = match parse_string exp str () with
  | Success exp -> exp
  | Failed (msg, _) ->
    Printf.eprintf "%s\n%!" msg; failwith msg

let from_file (fname : string) =
  let chan = open_in fname in
  match parse_channel exp chan () with
  | Success exp -> exp
  | Failed (msg, _) ->
    Printf.eprintf "%s\n%!" msg;
    failwith msg
