open Analyses

module type MAKE =
  functor (Typecheck:TYP) -> SYM

module Make : MAKE
