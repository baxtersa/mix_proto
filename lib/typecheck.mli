open Analyses

module type MAKE =
  functor (Sym:SYM) -> TYP

module Make : MAKE
