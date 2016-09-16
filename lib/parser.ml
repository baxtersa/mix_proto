open Ast
open MParser
open MParser_RE
open Tokens


type 'a parser = ('a, unit) MParser.t

let keywords = [
  "true"; "false"; "empty"; "head"; "tail"; "empty?";
  "if"; "then"; "else"; "let"; "in"; "fun"; "fix"
]

let id : string parser =
  attempt (((choice (List.map symbol keywords)) >>=
            fun x -> fail ("unexpected " ^ x ^ " (reserved keyword)")) <|>
           regexp (make_regexp "([A-Za-z_][A-Za-z_0-9_']*)") <<< spaces) (* >> *)

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
  [infix "+" Add];
  [infix "==" Eq; prefix "~" Neg; infix "/\\" Conj];
]

let rec typ_atom s = (
  parens typ <|>
  (symbol "int" |>> fun _ -> IntTyp) <|>
  (symbol "bool" |>> fun _ -> BoolTyp)
) s

and typ_ref s = (
  typ_atom >>= fun t -> (
    (skip_symbol "ref" |>> fun _ -> RefTyp t) <|>
    (return t))
) s

and typ s = (
  typ_ref >>= fun t1 -> (
    (symbol "->" >> typ |>> fun t2 -> FunTyp (t1, t2)) <|>
    (return t1))
) s

let rec atoms s = (
  parens exp <|>
  (symbol "true" |>> fun _ -> Const (Bool true)) <|>
  (symbol "false" |>> fun _ -> Const (Bool false)) <|>
  (decimal |>> (fun n -> Const (Int n))) <|>
  (id |>> (fun x -> Id x))
) s

and refs s = (
  (symbol "ref" >> exp |>> fun e -> Ref e) <|>
  (id) (symbol ":=" >> exp)
    (fun x e -> Assign (x, e))) <|>
  (symbol "!" >> id |>> fun x -> Deref x) <|>
  atoms
) s

and app s = (
    (many1 refs |>> rev_fold_left (fun x y -> App (y, x))) <|>
    (refs)
  ) s

and cmp s = expression operators app s

and exp s = (
    (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
       (fun e1 e2 e3 -> If (e1, e2, e3))) <|>
    (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
       (fun x e1 e2 -> Let (x, e1, e2))) <|>
    (pipe3 (symbol "fun" >> symbol "(" >> id) (symbol ":" >> typ) (symbol ")" >> symbol "->" >> exp)
       (fun x t e -> Fun (x, t, e))) <|>
    cmp
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
