open Ast
open MParser
open MParser_RE
open Tokens


type 'a parser = ('a, unit) MParser.t

let id : string parser = regexp (make_regexp "[A-Za-z_][A-Za-z_0-9_']*") <<< spaces

let infix sym op : (exp, unit) operator =
  let f e1 e2 = Binop (op, e1, e2) in
  Infix (skip_symbol sym >> return f, Assoc_left)

let prefix sym op : (exp, unit) operator =
  let f e = Unop (op, e) in
  Prefix (skip_symbol sym >> return f)

let operators : (exp, unit) operator list list = [
  [infix "+" Add];
  [infix "==" Eq; prefix "~" Neg; infix "/\\" Conj];
  [prefix "ref" Ref; infix ":=" Assign; prefix "!" Deref]
]

let rec atoms s = (
  parens exp <|>
  (symbol "true" |>> fun _ -> Const (Bool true)) <|>
  (symbol "false" |>> fun _ -> Const (Bool false)) <|>
  (decimal |>> (fun n -> Const (Int n))) <|>
  (id |>> (fun x -> Id x))
) s

and cmp s = expression operators atoms s

and exp s = (
  (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
     (fun e1 e2 e3 -> If (e1, e2, e3))) <|>
  (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
     (fun x e1 e2 -> Let (x, e1, e2))) <|>
  cmp
) s

let rev_fold_left f xs = match List.rev xs with
  | [] -> raise (Failure "expected at least one element (internal error)")
  | x :: xs -> List.fold_left f x xs

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
