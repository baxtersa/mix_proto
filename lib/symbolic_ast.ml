type sym_id = string [@@deriving show]

type sym_exp =
  | Typed of sym_exp * Ast.typ
  | SymId of sym_id
  | SymConst of Ast.const
  | SymBinop of Ast.binop * sym_exp * sym_exp
  | SymUnop of Ast.unop * sym_exp
  | MemSelect of sym_memory * sym_exp [@@deriving show]

and sym_memory =
  | Arbitrary
  | Update of sym_memory * sym_exp * sym_exp
  | Alloc of sym_memory * sym_exp * sym_exp [@@deriving show]

let count = ref 0
let fresh_sym () : sym_id =
  incr count;
  "_a" ^ (string_of_int !count)
