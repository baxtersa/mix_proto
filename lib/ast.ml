type id = string [@@deriving show]

type typ =
  | IntTyp
  | BoolTyp
  | RefTyp of typ
  | FunTyp of typ * typ [@@deriving show]

type binop =
  | Add
  | Eq
  | Conj [@@deriving show]

type unop =
  | Neg [@@deriving show]

type const =
  | Int of int
  | Bool of bool [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Binop of binop * exp * exp
  | Unop of unop * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Ref of exp
  | Assign of id * exp
  | Deref of id
  | Fun of id * typ * exp
  | App of exp * exp [@@deriving show]
