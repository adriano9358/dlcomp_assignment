(* This file contains the description of the calc language and some utils related to the AST *)

type calc_typee =
    | IntT
    | BoolT
    | FunT of calc_typee * calc_typee
    | RefT of calc_typee
    | UnitT
    | NoneT   


(* The abstract syntax tree (AST) type for the calc language *)
type ast = 
    Num of int
  | Bool of bool

  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

  | And of ast * ast
  | Or of ast * ast
  | Not of ast

  | Eq of ast * ast
  | Neq of ast * ast
  | Lt of ast * ast
  | Le of ast * ast
  | Gt of ast * ast
  | Ge of ast * ast
  | Neg of ast

  | Let of (string * ast) list * ast
  | Id of string

  | Seq of ast * ast                 (* E1 ; E2 *)
  | Assign of ast * ast              (* E1 := E2 *)
  | If of ast * ast * ast            (* if E1 then E2 else E3 *)
  | While of ast * ast               (* while E1 do E2 *)

  | New of ast                       (* new(E) *)
  | Deref of ast                     (* !E *)
  | Free of ast                      (* free(E) *)

  | PrintInt of ast
  | PrintBool of ast
  | PrintEndline

  | Fun of string * calc_typee * ast            
  | App of ast * ast



let paren = fun p q s -> if p > q then "("^s^")" else s

(* This function converts an AST back to a string representation of the expression *)
let rec unparse_ast p e = 
  match e with
  | Num x -> string_of_int x
  | Bool b -> string_of_bool b
  | Add (e1,e2) -> paren p 10 (unparse_ast 10 e1 ^ " + " ^ unparse_ast 10 e2)
  | Sub (e1,e2) -> paren p 10 (unparse_ast 10 e1 ^ " - " ^ unparse_ast 11 e2)
  | Mul (e1,e2) -> paren p 30 (unparse_ast 20 e1 ^ " * " ^ unparse_ast 20 e2)
  | Div (e1,e2) -> paren p 20 (unparse_ast 20 e1 ^ " / " ^ unparse_ast 21 e2)
  | Neg e1 -> paren p 30 ("-"^unparse_ast 31 e1)
  | And (e1,e2) -> paren p 5 (unparse_ast 5 e1 ^ " && " ^ unparse_ast 6 e2)
  | Or (e1,e2) -> paren p 3 (unparse_ast 3 e1 ^ " || " ^ unparse_ast 4 e2)
  | Not e1 -> paren p 30 ("not " ^ unparse_ast 31 e1)
  | Eq (e1,e2) -> paren p 7 (unparse_ast 7 e1 ^ " == " ^ unparse_ast 8 e2)
  | Neq (e1,e2) -> paren p 7 (unparse_ast 7 e1 ^ " != " ^ unparse_ast 8 e2)
  | Lt (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " < " ^ unparse_ast 10 e2)
  | Le (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " <= " ^ unparse_ast 10 e2)
  | Gt (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " > " ^ unparse_ast 10 e2)
  | Ge (e1,e2) -> paren p 9 (unparse_ast 9 e1 ^ " >= " ^ unparse_ast 10 e2)
  | Let (l,e2) -> 
      let decls = String.concat " " (List.map (fun (x,e) -> x^" = "^unparse_ast 0 e) l) in
      "let "^decls^" in "^unparse_ast 0 e2
  | Id x -> x

  | Seq (e1,e2) -> paren p 1 (unparse_ast 1 e1 ^ " ; " ^ unparse_ast 0 e2)
  | Assign (e1,e2) -> paren p 15 (unparse_ast 15 e1 ^ " := " ^ unparse_ast 16 e2)
  | If (e1,e2,e3) -> 
      paren p 2 ("if " ^ unparse_ast 0 e1 ^ " then " ^ unparse_ast 0 e2 ^ " else " ^ unparse_ast 0 e3)
  | While (e1,e2) -> 
      paren p 2 ("while " ^ unparse_ast 0 e1 ^ " do " ^ unparse_ast 0 e2)

  | New e1 -> paren p 25 ("new(" ^ unparse_ast 0 e1 ^ ")")
  | Deref e1 -> paren p 25 ("!" ^ unparse_ast 26 e1)
  | Free e1 -> paren p 25 ("free(" ^ unparse_ast 0 e1 ^ ")")

  | PrintInt e1 -> paren p 0 ("printInt(" ^ unparse_ast 0 e1 ^ ")")
  | PrintBool e1 -> paren p 0 ("printBool(" ^ unparse_ast 0 e1 ^ ")")
  | PrintEndline -> paren p 0 "printEndline()"

  | Fun (arg,arg_type,body) -> 
    let type_str = 
        match arg_type with
        | IntT -> "int"
        | BoolT -> "bool"
        | UnitT -> "unit"
        | RefT (_) -> "ref_type"
        | FunT (_,_) -> "fun_type"  
        | NoneT -> "none"
    in
    paren p 0 ("fun (" ^ arg ^ " : " ^ type_str ^ ") -> " ^ unparse_ast 0 body)
  
    | App (e1,e2) -> paren p 20 (unparse_ast 20 e1 ^ " (" ^ unparse_ast 0 e2 ^ ")")
    (* | _ -> assert false   *)


