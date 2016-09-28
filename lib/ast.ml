type id = string [@@deriving show]

type typ =
  | TInt
  | TBool
  | TRef of typ
  | TFun of typ * typ [@@deriving show]

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
  | Assign of exp * exp
  | Deref of exp
  | Fun of id * typ * exp
  | App of exp * exp
  | TypedBlock of exp
  | SymbolicBlock of exp [@@deriving show]
