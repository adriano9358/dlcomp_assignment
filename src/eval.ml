(* This file contains the definitional interpreter for the calc language *)

open Ast
open Env
open Mem

type result = 
  | IntV of int
  | BoolV of bool
  | RefV of result ref
  | UnitV
  | ClosureV of string * Ast.ast * result Env.env

let unparse_result = function
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | RefV _ -> "<ref>"
  | UnitV -> "unit"
  | ClosureV _ -> "<closure>"

let int_int_binop f r1 r2 = 
  match r1, r2 with
  | IntV n1, IntV n2 -> IntV (f n1 n2)
  | _ -> failwith "Runtime typing error"

let bool_bool_binop f r1 r2 = 
  match r1, r2 with
  | BoolV n1, BoolV n2 -> BoolV (f n1 n2)
  | _ -> failwith "Runtime typing error"

let a_a_bool_eq r1 r2 = 
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 = n2)
  | BoolV n1, BoolV n2 -> BoolV (n1 = n2)
  | _ -> failwith "Runtime typing error"

let a_a_bool_neq r1 r2 = 
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 <> n2)
  | BoolV n1, BoolV n2 -> BoolV (n1 <> n2)
  | _ -> failwith "Runtime typing error"


(* Could potentially reduce the following functions to a more generalized version*)

let int_int_bool_lt r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 < n2)
  | _ -> failwith "Runtime typing error"

let int_int_bool_le r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 <= n2)
  | _ -> failwith "Runtime typing error"

let int_int_bool_gt r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 > n2)
  | _ -> failwith "Runtime typing error"

let int_int_bool_ge r1 r2 =
  match r1, r2 with
  | IntV n1, IntV n2 -> BoolV (n1 >= n2)
  | _ -> failwith "Runtime typing error"


let as_int = function
  | IntV n -> n
  | _ -> failwith "Runtime typing error: expected integer"

let as_bool = function
  | BoolV b -> b
  | _ -> failwith "Runtime typing error: expected boolean"

let rec eval e env =
  match e with  
  | Num n -> IntV n
  | Bool b -> BoolV b
  | Add (e1,e2) -> int_int_binop ( + ) (eval e1 env) (eval e2 env)
  | Sub (e1,e2) -> int_int_binop ( - ) (eval e1 env) (eval e2 env)
  | Mul (e1,e2) -> int_int_binop ( * ) (eval e1 env) (eval e2 env)
  | Div (e1,e2) -> int_int_binop ( / ) (eval e1 env) (eval e2 env)
  | Neg e1 ->  int_int_binop (-) (IntV 0) (eval e1 env)
  | And (e1,e2) -> bool_bool_binop (&&) (eval e1 env) (eval e2 env)
  (* | And (e1,e2) -> begin match eval e1 with 
                     | BoolV true -> eval e2
                     | _ -> BoolV false
                   end *)
  | Or (e1,e2) -> bool_bool_binop (||) (eval e1 env) (eval e2 env)
  | Eq (e1,e2) -> a_a_bool_eq (eval e1 env) (eval e2 env)
  | Neq (e1,e2) -> a_a_bool_neq (eval e1 env) (eval e2 env)
  | Lt (e1,e2) -> int_int_bool_lt (eval e1 env) (eval e2 env)
  | Le (e1,e2) -> int_int_bool_le (eval e1 env) (eval e2 env)
  | Gt (e1,e2) -> int_int_bool_gt (eval e1 env) (eval e2 env)
  | Ge (e1,e2) -> int_int_bool_ge (eval e1 env) (eval e2 env)
  | Not e1 -> 
      (match eval e1 env with
       | BoolV b -> BoolV (not b)
       | _ -> failwith "Runtime typing error: not requires boolean")
  | If (cond, then_branch, else_branch) ->
      begin match eval cond env with
      | BoolV true -> eval then_branch env
      | BoolV false -> eval else_branch env
      | _ -> failwith "Runtime typing error: if condition must be boolean"
      end
  | Id x -> let a = lookup env x in
            begin match a with
            | Some v -> v
            | None -> failwith ("Unbound variable: " ^ x)
            end

  | Let (bindings, body) ->
      let env' = begin_scope env in
      let env_after_bindings =
        List.fold_left
          (fun acc_env (name, expr) ->
             let v = eval expr acc_env in        (* evaluate initializer in current acc_env *)
             let r = new_ref v in                (* allocate a new memory cell and initialize *)
             bind acc_env name (RefV r)         (* bind name -> RefV r in current scope *)
          )
          env'
          bindings
      in
      eval body env_after_bindings

  | Seq (e1, e2) ->
      let _ = eval e1 env in
      eval e2 env

  | New e1 ->
      let v = eval e1 env in
      RefV (ref v)

  | Deref e1 ->
      (match eval e1 env with
       | RefV r -> !r
       | _ -> failwith "Runtime error: deref expects a reference")

  | Assign (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
       | RefV r -> r := v2; UnitV
       | _ -> failwith "Runtime error: assign expects a reference")

  | Free e1 ->
      (match eval e1 env with
       | RefV _ -> UnitV   (* optional: you could mark as "freed" if you want *)
       | _ -> failwith "Runtime error: free expects a reference")

  | Fun (param, body) ->
      ClosureV (param, body, env)

  | App (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
       | ClosureV (param, body, env0) -> 
           let env1 = begin_scope env0 in
           let env1' = bind env1 param v2 in
           let result = eval body env1' in
           let _ = end_scope env1 in
           result
       | _ -> failwith "Runtime error: application of a non-function")

  | _ -> assert false 