open Typing

type const_val =
  | CInt of int
  | CBool of bool

let rec lookup_const x env =
  match env with
  | [] -> None
  | (y, v) :: tl ->
      if x = y then Some v else lookup_const x tl

(* env : (string * const_val) list *)
let rec optimize_with_env env e =
  match e with
  | Num _ -> e
  | Bool _ -> e

  | Add (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Num (n1 + n2)
       | _ -> Add (t, e1', e2'))

  | Sub (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Num (n1 - n2)
       | _ -> Sub (t, e1', e2'))

  | Mul (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Num (n1 * n2)
       | _ -> Mul (t, e1', e2'))

  | Div (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num _, Num 0 ->
           Div (t, e1', e2')
       | Num n1, Num n2 -> Num (n1 / n2)
       | _ -> Div (t, e1', e2'))

  | Neg (t, e1) ->
      let e1' = optimize_with_env env e1 in
      (match e1' with
       | Num n -> Num (- n)
       | _ -> Neg (t, e1'))

  | And (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _ -> And (t, e1', e2'))

  | Or (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Bool b1, Bool b2 -> Bool (b1 || b2)
       | _ -> Or (t, e1', e2'))

  | Not (t, e1) ->
      let e1' = optimize_with_env env e1 in
      (match e1' with
       | Bool b -> Bool (not b)
       | _ -> Not (t, e1'))

  | Eq (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 = n2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _ -> Eq (t, e1', e2'))

  | Neq (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 <> n2)
       | Bool b1, Bool b2 -> Bool (b1 <> b2)
       | _ -> Neq (t, e1', e2'))

  | Lt (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 < n2)
       | _ -> Lt (t, e1', e2'))

  | Le (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 <= n2)
       | _ -> Le (t, e1', e2'))

  | Gt (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 > n2)
       | _ -> Gt (t, e1', e2'))

  | Ge (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      (match e1', e2' with
       | Num n1, Num n2 -> Bool (n1 >= n2)
       | _ -> Ge (t, e1', e2'))

  | Id (t, x) ->
      (match lookup_const x env with
       | Some (CInt n) -> Num n
       | Some (CBool b) -> Bool b
       | None -> Id (t, x))

  | Let (t, bindings, body) ->
      let rec opt_bindings env_acc binds =
        match binds with
        | [] -> (env_acc, [])
        | (name, expr) :: rest ->
            let expr' = optimize_with_env env_acc expr in
            let env_acc' =
              match expr' with
              | Num n -> (name, CInt n) :: env_acc
              | Bool b -> (name, CBool b) :: env_acc
              | _ -> env_acc
            in
            let env_final, rest' = opt_bindings env_acc' rest in
            (env_final, (name, expr') :: rest')
      in
      let env_for_body, bindings' = opt_bindings env bindings in
      let body' = optimize_with_env env_for_body body in
      Let (t, bindings', body')

  | Seq (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      Seq (t, e1', e2')

  | Assign (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      Assign (t, e1', e2')

  | If (t, econd, ethen, eelse) ->
      let econd' = optimize_with_env env econd in
      let ethen' = optimize_with_env env ethen in
      let eelse' = optimize_with_env env eelse in
      (match econd' with
       | Bool true -> ethen'
       | Bool false -> eelse'
       | _ -> If (t, econd', ethen', eelse'))

  | While (t, econd, ebody) ->
      let econd' = optimize_with_env env econd in
      let ebody' = optimize_with_env env ebody in
      While (t, econd', ebody')

  | New (t, e1) ->
      let e1' = optimize_with_env env e1 in
      New (t, e1')

  | Deref (t, e1) ->
      let e1' = optimize_with_env env e1 in
      Deref (t, e1')

  | Free (t, e1) ->
      let e1' = optimize_with_env env e1 in
      Free (t, e1')

  | PrintInt (t, e1) ->
      let e1' = optimize_with_env env e1 in
      PrintInt (t, e1')

  | PrintBool (t, e1) ->
      let e1' = optimize_with_env env e1 in
      PrintBool (t, e1')

  | PrintEndline t ->
      PrintEndline t

  | Fun (t, name, param_ty, body) ->
      let body' = optimize_with_env [] body in
      Fun (t, name, param_ty, body')

  | App (t, e1, e2) ->
      let e1' = optimize_with_env env e1 in
      let e2' = optimize_with_env env e2 in
      App (t, e1', e2')

let optimize e =
  optimize_with_env [] e