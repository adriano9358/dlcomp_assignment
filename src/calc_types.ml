(* types.ml *)

type calc_type = 
  | IntT 
  | BoolT
  | RefT of calc_type
  | UnitT
  | NoneT of string
  | FunT of calc_type * calc_type