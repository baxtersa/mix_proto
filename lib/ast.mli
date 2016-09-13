type id = string [@@deriving show]

type binop =
  | Add
  | Eq
  | Conj
  | Assign

type unop =
  | Neg
  | Deref

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
  | TypeBlock of exp
  | SymbolicBloc of exp
  | Ref of exp

type typ =
  | IntTyp
  | BoolTyp
  | RefTyp of typ [@@deriving show]
