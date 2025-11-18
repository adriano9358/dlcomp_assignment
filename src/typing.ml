(* typing.ml *)
(* This file contains the type system for the calcb language, with typed annotations *)

open Env

type calc_type = 
  | IntT 
  | BoolT
  | RefT of calc_type
  | UnitT
  | NoneT of string

type ann = calc_type

type ast = 
    Num of int
  | Bool of bool

  | Add of ann * ast * ast
  | Sub of ann * ast * ast
  | Mul of ann * ast * ast
  | Div of ann * ast * ast
  | Neg of ann * ast

  | And of ann * ast * ast
  | Or of ann * ast * ast
  | Not of ann * ast

  | Eq of ann * ast * ast
  | Neq of ann * ast * ast
  | Lt of ann * ast * ast
  | Le of ann * ast * ast
  | Gt of ann * ast * ast
  | Ge of ann * ast * ast

  | Id of ann * string
  | Let of ann * (string * ast) list * ast

  | Seq of ann * ast * ast
  | Assign of ann * ast * ast
  | If of ann * ast * ast * ast
  | While of ann * ast * ast

  | New of ann * ast
  | Deref of ann * ast
  | Free of ann * ast

  | PrintInt of ann * ast
  | PrintBool of ann * ast
  | PrintEndline of ann


let type_of = function
  | Num _ -> IntT
  | Bool _ -> BoolT
  | Add (t,_,_) | Sub (t,_,_) | Mul (t,_,_) | Div (t,_,_) | Neg (t,_) 
  | And (t,_,_) | Or (t,_,_) | Not (t,_) 
  | Eq (t,_,_) | Neq (t,_,_) | Lt (t,_,_) | Le (t,_,_) | Gt (t,_,_) | Ge (t,_,_)
  | Id (t,_) | Let (t,_,_)
  | Seq (t,_,_) | Assign (t,_,_) | If (t,_,_,_) | While (t,_,_)
  | New (t,_) | Deref (t,_) | Free (t,_)
  | PrintInt (t,_) | PrintBool (t,_) | PrintEndline(t) -> t

let rec unparse_type = function
  | IntT -> "int"
  | BoolT -> "boolean"
  | RefT t -> "ref(" ^ unparse_type t ^ ")"
  | UnitT -> "unit"
  | NoneT m -> "typing error: " ^ m


let mk_add t e1 e2 = Add (t, e1, e2)
let mk_sub t e1 e2 = Sub (t, e1, e2)
let mk_mul t e1 e2 = Mul (t, e1, e2)
let mk_div t e1 e2 = Div (t, e1, e2)
let mk_neg t e1 = Neg (t, e1)

let mk_and t e1 e2 = And (t, e1, e2)
let mk_or  t e1 e2 = Or  (t, e1, e2)
let mk_not t e1 = Not (t, e1)

let mk_eq  t e1 e2 = Eq  (t, e1, e2)
let mk_neq t e1 e2 = Neq (t, e1, e2)
let mk_lt  t e1 e2 = Lt  (t, e1, e2)
let mk_le  t e1 e2 = Le  (t, e1, e2)
let mk_gt  t e1 e2 = Gt  (t, e1, e2)
let mk_ge  t e1 e2 = Ge  (t, e1, e2)

let mk_id  t x = Id (t, x)
let mk_let t binds body = Let (t, binds, body)

let mk_seq t e1 e2 = Seq (t, e1, e2)
let mk_assign t e1 e2 = Assign (t, e1, e2)
let mk_if t e1 e2 e3 = If (t, e1, e2, e3)
let mk_while t e1 e2 = While (t, e1, e2)
let mk_new t e = New (t, e)
let mk_deref t e = Deref (t, e)
let mk_free t e = Free (t, e)
let mk_printint t e = PrintInt (t, e)
let mk_printbool t e = PrintBool (t, e)
let mk_printendline t = PrintEndline (t)


let type_int_int_int_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | IntT, IntT -> mk IntT e1 e2
  | _ -> mk (NoneT "Expecting Integer") e1 e2

let type_int_int_bin_op mk e1 =
  match type_of e1 with
  | IntT -> mk IntT e1
  | _ -> mk (NoneT "Expecting Integer") e1

let type_bool_bool_bool_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | BoolT, BoolT -> mk BoolT e1 e2
  | _ -> mk (NoneT "Expecting Boolean") e1 e2

let type_int_int_bool_bin_op mk e1 e2 =
  match type_of e1, type_of e2 with
  | IntT, IntT -> mk BoolT e1 e2
  | _ -> mk (NoneT "Expecting Integer") e1 e2

let type_a_a_bool_eqop mk e1 e2 =
  if type_of e1 = type_of e2 
  then mk BoolT e1 e2 
  else mk (NoneT "Expecting equal types") e1 e2


let rec typecheck e env =
  match e with  
  | Ast.Num n -> Num n
  | Ast.Bool b -> Bool b

  | Ast.Add (e1, e2) -> type_int_int_int_bin_op mk_add (typecheck e1 env) (typecheck e2 env)
  | Ast.Sub (e1, e2) -> type_int_int_int_bin_op mk_sub (typecheck e1 env) (typecheck e2 env)
  | Ast.Mul (e1, e2) -> type_int_int_int_bin_op mk_mul (typecheck e1 env) (typecheck e2 env)
  | Ast.Div (e1, e2) -> type_int_int_int_bin_op mk_div (typecheck e1 env) (typecheck e2 env)
  | Ast.Neg e1 -> type_int_int_bin_op mk_neg (typecheck e1 env)

  | Ast.And (e1, e2) -> type_bool_bool_bool_bin_op mk_and (typecheck e1 env) (typecheck e2 env)
  | Ast.Or  (e1, e2) -> type_bool_bool_bool_bin_op mk_or  (typecheck e1 env) (typecheck e2 env)
  | Ast.Not e1 ->
      let e1' = typecheck e1 env in
      (match type_of e1' with
       | BoolT -> mk_not BoolT e1'
       | _ -> mk_not (NoneT "Not requires boolean") e1')

  | Ast.Eq (e1, e2)  -> type_a_a_bool_eqop mk_eq (typecheck e1 env) (typecheck e2 env)
  | Ast.Neq (e1, e2) -> type_a_a_bool_eqop mk_neq (typecheck e1 env) (typecheck e2 env)
  | Ast.Lt (e1, e2)  -> type_int_int_bool_bin_op mk_lt (typecheck e1 env) (typecheck e2 env)
  | Ast.Le (e1, e2)  -> type_int_int_bool_bin_op mk_le (typecheck e1 env) (typecheck e2 env)
  | Ast.Gt (e1, e2)  -> type_int_int_bool_bin_op mk_gt (typecheck e1 env) (typecheck e2 env)
  | Ast.Ge (e1, e2)  -> type_int_int_bool_bin_op mk_ge (typecheck e1 env) (typecheck e2 env)

  | Ast.Id x ->
      (match lookup env x with
       | Some t -> mk_id t x
       | None -> mk_id (NoneT ("Unbound identifier: " ^ x)) x)

  | Ast.Let (bindings, body) ->
      let env' = begin_scope env in
      let env_after_bindings =
        List.fold_left (fun acc_env (x, e) ->
            let te = typecheck e acc_env in
            bind acc_env x (type_of te)
          ) env' bindings
      in
      let typed_bindings = List.map (fun (x, e) -> (x, typecheck e env_after_bindings)) bindings in
      let typed_body = typecheck body env_after_bindings in
      mk_let (type_of typed_body) typed_bindings typed_body

  | Ast.New e1 ->
      let e1' = typecheck e1 env in
      mk_new (RefT (type_of e1')) e1'

  | Ast.Deref e1 ->
      let e1' = typecheck e1 env in
      (match type_of e1' with
       | RefT t -> mk_deref t e1'
       | NoneT m -> mk_deref (NoneT m) e1'
       | _ -> mk_deref (NoneT "Dereference expects a reference") e1')

  | Ast.Assign (e1, e2) ->
      let e1' = typecheck e1 env in
      let e2' = typecheck e2 env in
      (match type_of e1', type_of e2' with
       | RefT t1, t2 when t1 = t2 -> mk_assign t2 e1' e2'
       | RefT _, NoneT m -> mk_assign (NoneT m) e1' e2'
       | NoneT m, _ -> mk_assign (NoneT m) e1' e2'
       | _ -> mk_assign (NoneT "Assignment requires Ref(lhs) and matching rhs type") e1' e2')

  | Ast.Seq (e1, e2) ->
      let e1' = typecheck e1 env in
      let e2' = typecheck e2 env in
      mk_seq (type_of e2') e1' e2'

  | Ast.If (e1, e2, e3) ->
      let e1' = typecheck e1 env in
      let e2' = typecheck e2 env in
      let e3' = typecheck e3 env in
      (match type_of e1' with
       | BoolT ->
           let t2, t3 = type_of e2', type_of e3' in
           if t2 = t3 then mk_if t2 e1' e2' e3'
           else mk_if (NoneT "If branches have mismatched types") e1' e2' e3'
       | _ -> mk_if (NoneT "If condition must be boolean") e1' e2' e3')

  | Ast.While (e1, e2) ->
      let e1' = typecheck e1 env in
      let e2' = typecheck e2 env in
      (match type_of e1' with
       | BoolT ->
           (match type_of e2' with
            | NoneT m -> mk_while (NoneT m) e1' e2'
            | _ -> mk_while UnitT e1' e2')
       | _ -> mk_while (NoneT "While condition must be boolean") e1' e2')

  | Ast.Free e1 ->
      let e1' = typecheck e1 env in
      (match type_of e1' with
       | RefT _ -> mk_free UnitT e1'
       | _ -> mk_free (NoneT "Free expects a reference") e1')

  | Ast.PrintInt e1 ->
      let e1' = typecheck e1 env in
      (match type_of e1' with
       | IntT -> mk_printint UnitT e1'
       | _ -> mk_printint (NoneT "printInt expects integer") e1')

  | Ast.PrintBool e1 ->
      let e1' = typecheck e1 env in
      (match type_of e1' with
       | BoolT -> mk_printbool UnitT e1'
       | _ -> mk_printbool (NoneT "printBool expects boolean") e1')

  | Ast.PrintEndline ->
      mk_printendline UnitT

  | _ -> failwith "Unimplemented typing case"
      
