open Typing

type register = int
type label = int

type result =
  | Const of int
  | Register of register

type llvm =
  | Addi32 of register * result * result
  | Subi32 of register * result * result
  | Muli32 of register * result * result
  | Divi32 of register * result * result
  | Negi32 of register * result
  | CmpEq of Typing.calc_type * register * result * result
  | CmpNe of Typing.calc_type * register * result * result
  | CmpLt of Typing.calc_type * register * result * result
  | CmpLe of Typing.calc_type * register * result * result
  | CmpGt of Typing.calc_type * register * result * result
  | CmpGe of Typing.calc_type * register * result * result
  | BrI1 of result * label * label
  | BrLabel of label
  | PhiI1 of register * (result * label) list

let count = ref 0
let new_reg () = incr count; !count
let new_label = new_reg



let rec compile_llvm e env label block = 
  match e with
  | Num x -> Const x, label, block, []
  | Bool b when b = true -> Const 1, label, block, []
  | Bool b when b = false -> Const 0, label, block, []
  (* | Bool b -> if b then Const 1, label, block, [] else Const 0, label, block, [] *)

  | Add (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [Addi32 (ret, r1, r2)], bs1 @ bs2)
  | Sub (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [Subi32 (ret, r1, r2)], bs1 @ bs2)
  | Mul (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [Muli32 (ret, r1, r2)], bs1 @ bs2)
  | Div (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [Divi32 (ret, r1, r2)], bs1 @ bs2)
  | Neg (_, e1) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let ret = new_reg () in
      (Register ret, l1, b1 @ [Negi32 (ret, r1)], bs1)
  | And (_, e1, e2) ->
      (* Short-circuit AND:
         r1 = compile e1
         if r1 then goto label_b else goto label_phi
         label_b: compile e2 -> r2 ; br label_phi
         label_phi: phi [ (r1,l1), (r2,l2) ]
      *)
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let label_b = new_label () in
      let r2, l2, b2, bs2 = compile_llvm e2 env label_b [] in
      let label_phi = new_label () in
      let blocks_combined =
        bs1
        @ [ (l1, b1 @ [BrI1 (r1, label_b, label_phi)]) ]
        @ bs2
        @ [ (l2, b2 @ [BrLabel label_phi]) ]
      in
      let ret = new_reg () in
      (Register ret, label_phi, [PhiI1 (ret, [ (r1, l1); (r2, l2) ])], blocks_combined)
  | Or (_, e1, e2) ->
      (* Short-circuit OR:
         if r1 then goto label_phi else goto label_b
         label_b: compile e2; br label_phi
         label_phi: phi [(r1,l1), (r2,l2)]
      *)
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let label_b = new_label () in
      let r2, l2, b2, bs2 = compile_llvm e2 env label_b [] in
      let label_phi = new_label () in
      let blocks_combined =
        bs1
        @ [ (l1, b1 @ [BrI1 (r1, label_phi, label_b)]) ]
        @ bs2
        @ [ (l2, b2 @ [BrLabel label_phi]) ]
      in
      let ret = new_reg () in
      (Register ret, label_phi, [PhiI1 (ret, [ (r1, l1); (r2, l2) ])], blocks_combined)
  | Eq (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpEq (type_of e1, ret, r1, r2)], bs1 @ bs2)
  | Neq (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpNe (type_of e1, ret, r1, r2)], bs1 @ bs2)

  | Lt (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpLt (type_of e1, ret, r1, r2)], bs1 @ bs2)

  | Le (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpLe (type_of e1, ret, r1, r2)], bs1 @ bs2)

  | Gt (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpGt (type_of e1, ret, r1, r2)], bs1 @ bs2)

  | Ge (_, e1, e2) ->
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
      let ret = new_reg () in
      (Register ret, l2, b2 @ [CmpGe (type_of e1, ret, r1, r2)], bs1 @ bs2)
  | Not (_, e1) ->
      (* Not e  ==>  (e == 0)  (produces i1)
         if e1 is constant, fold; otherwise emit icmp eq (i1 or i32) with 0.
      *)
      let r1, l1, b1, bs1 = compile_llvm e1 env label block in
      (match r1 with
       | Const c -> if c = 0 then (Const 1, l1, b1, bs1) else (Const 0, l1, b1, bs1)
       | Register _ ->
           let ret = new_reg () in
           (* Use CmpEq with BoolT (i1). If the operand was i32 (IntT) then
              Typing.type_of of e1 should reflect that; here we use type_of e1. *)
           (Register ret, l1, b1 @ [CmpEq (type_of e1, ret, r1, Const 0)], bs1)
      )

  | Id (_,x) -> 
      begin match Env.lookup env x with 
      | None -> failwith ("Unbound identifier: "^x) 
      | Some r -> (r, label, block, []) 
      end

  (* | Let (_, bindings, body) ->
      (* Evaluate bindings sequentially; each binding can see previous ones.
         Accumulate blocks and update env with the result (Const or Register).
      *)
      let rec eval_bindings binds acc_env cur_label cur_block acc_blocks =
        match binds with
        | [] -> (acc_env, cur_label, cur_block, acc_blocks)
        | (name, expr) :: rest ->
            let res, l', b', bs' = compile_llvm expr acc_env cur_label cur_block in
            let acc_blocks' = acc_blocks @ bs' in
            (* Continue from last label/block returned *)
            eval_bindings rest ((name, res) :: acc_env) l' b' acc_blocks'
      in
      let env_after, l_after, b_after, bs_after = eval_bindings bindings env label block [] in
      (* Now compile body with env_after *)
      let res_body, l_body, b_body, bs_body = compile_llvm body env_after l_after b_after in
      (res_body, l_body, b_body, bs_after @ bs_body) *)

  (* | Let(_, [x,e], body) ->
    let r1,l1,b1,bs1 = compile_llvm e env label block in
    let env' = Env.begin_scope env in
    let env'' = Env.bind env' x r1 in
    let r2,l2,b2,bs2 = compile_llvm body env'' l1 b1 in
    let _ = Env.end_scope env'' in
    (r2, l2, b2, bs1@bs2)
  
  | Let(_,_,_) -> failwith "Multiple bindings in let not yet implemented" *)

  | Let(_, bindings, body) ->
    (* Start a new scope *)
    let env' = Env.begin_scope env in

    (* Sequentially evaluate each binding *)
    let rec eval_bindings binds acc_env cur_label cur_block acc_blocks =
      match binds with
      | [] -> (acc_env, cur_label, cur_block, acc_blocks)
      | (name, expr) :: rest ->
          let r, l', b', bs' = compile_llvm expr acc_env cur_label cur_block in
          let acc_env' = Env.bind acc_env name r in
          eval_bindings rest acc_env' l' b' (acc_blocks @ bs')
    in

    let env_after, l_after, b_after, bs_after =
      eval_bindings bindings env' label block []
    in

    (* Compile the body with the updated environment *)
    let r_body, l_body, b_body, bs_body =
      compile_llvm body env_after l_after b_after
    in

    (* End the scope *)
    let _ = Env.end_scope env_after in

    (* Return the body’s result and combined blocks *)
    (r_body, l_body, b_body, bs_after @ bs_body)


  | _ -> failwith "internal: unexpected llvm node in typing AST"




(* === Unparse / print helpers === *)

let prologue =
  ["@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1";
   "define i32 @main() #0 {"]

let epilogue =
  ["  ret i32 0";
   "}";
   "declare i32 @printf(i8* noundef, ...) #1"]

let unparse_register n = "%" ^ string_of_int n
let unparse_label_use n = "%" ^ string_of_int n
let unparse_label_declaration l = (string_of_int l) ^ ":"

let unparse_result = function
  | Const x -> string_of_int x
  | Register x -> unparse_register x

let unparse_type = function
  | IntT -> "i32"
  | BoolT -> "i1"
  | _ -> failwith "Unknown type"

let unparse_llvm_i = function
  | Addi32 (r, l1, l2) ->
      "  " ^ unparse_register r ^ " = add nsw i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | Subi32 (r, l1, l2) ->
      "  " ^ unparse_register r ^ " = sub nsw i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | Muli32 (r, l1, l2) ->
      "  " ^ unparse_register r ^ " = mul nsw i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | Divi32 (r, l1, l2) ->
      "  " ^ unparse_register r ^ " = sdiv i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | Negi32 (r, p) ->
      "  " ^ unparse_register r ^ " = sub nsw i32 0, " ^ unparse_result p
  | BrI1 (r, l1, l2) ->
      "  br i1 " ^ unparse_result r ^ ", label " ^ unparse_label_use l1 ^ ", label " ^ unparse_label_use l2
  | BrLabel label ->
      "  br label " ^ unparse_label_use label
  | PhiI1 (r, l) ->
      "  " ^ unparse_register r ^ " = phi i1 " ^
      String.concat ", " (List.map (fun (r, l) -> "[" ^ unparse_result r ^ ", " ^ unparse_label_use l ^ "]") l)
  | CmpEq (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp eq i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpEq (BoolT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp eq i1 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpEq (t, _, _, _) -> failwith ("Internal error: Cannot compare " ^ (unparse_type t))
    | CmpNe (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp ne i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpNe (BoolT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp ne i1 " ^ unparse_result l1 ^ ", " ^ unparse_result l2

  | CmpLt (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp slt i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpLe (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp sle i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpGt (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp sgt i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2
  | CmpGe (IntT, r, l1, l2) ->
      "  " ^ unparse_register r ^ " = icmp sge i32 " ^ unparse_result l1 ^ ", " ^ unparse_result l2

  | CmpLt (BoolT, _r, _l1, _l2)
  | CmpLe (BoolT, _r, _l1, _l2)
  | CmpGt (BoolT, _r, _l1, _l2)
  | CmpGe (BoolT, _r, _l1, _l2) ->
      failwith "Internal error: comparison <, <=, >, >= not valid for bool"

  | CmpNe (t, _, _, _)
  | CmpLt (t, _, _, _)
  | CmpLe (t, _, _, _)
  | CmpGt (t, _, _, _)
  | CmpGe (t, _, _, _) ->
      failwith ("Internal error: Cannot compare type " ^ (unparse_type t))

  
  (* | _ -> failwith "Not implemented yet" *)

let print_block (label, instructions) =
  print_endline (unparse_label_declaration label);
  List.iter (fun x -> x |> unparse_llvm_i |> print_endline) instructions

let print_blocks bs = List.iter print_block bs

let emit_printf ret t =
  let llvm_type = unparse_type t in
  "  " ^ unparse_register (new_reg ()) ^ " = call i32 (i8*, ...) @printf(i8* noundef @.str, " ^
  llvm_type ^ " noundef " ^ unparse_result ret ^ ")"

let print_llvm (ret, label, instructions, blocks) t =
  List.iter print_endline prologue;
  print_blocks (blocks @ [(label, instructions)]);
  print_endline (emit_printf ret t);
  List.iter print_endline epilogue

let compile e = compile_llvm e [] 0 []
