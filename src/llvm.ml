open Typing

type register = int
type label = int

type result =
  | Const of int
  | Register of register
  | Ptr of register
  | FunPtr of string

type mem_kind = IntK | BoolK | RefK

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
  | PhiI32 of register * (result * label) list
  | ZextI1ToI32 of register * result
  | CallPrintfI32 of register * result
  | CallNew of mem_kind * register * result
  | CallLoad of mem_kind * register * result
  | CallStore of mem_kind * result * result
  | CallFree of result
  | CallFunI32 of register * string * Typing.calc_type * result
  | CallFunI1 of register * string * Typing.calc_type * result
  | CallFunPtr of register * string * Typing.calc_type * result
  | CallFunVoid of string * Typing.calc_type * result
  | RetI32 of result
  | RetI1 of result
  | RetPtr of result
  | RetVoid

let count = ref 0
let new_reg () = incr count; !count
let new_label = new_reg                                                 (*estou com o pequeno pressentimento que isto é inútil e pode-se meter tudo new_reg*)

type func_def = {
  fname : string;
  param_name : string;
  ret_type : Typing.calc_type;
  param_type : Typing.calc_type;
  blocks : (label * llvm list) list;
  entry_label : label;
}
let functions : func_def list ref = ref []
let new_fun_id =
  let c = ref 0 in
  fun () -> incr c; !c

let llvm_type_of_param = function
  | Typing.IntT -> "i32"
  | Typing.BoolT -> "i1"
  | Typing.RefT _ -> "ptr"
  | Typing.FunT _ -> "ptr"
  | Typing.UnitT -> "unit"    (* unit represented as a dummy integer *)
  | Typing.NoneT m ->
      failwith ("invalid parameter type: " ^ m)


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
       | _ -> failwith "Unexpected result type in Not"
      )

  | If (t_if, econd, ethen, eelse) ->
    (* condition in i1 *)
    let rcond, l0, b0, bs0 = compile_llvm econd env label block in
    let l_then  = new_label () in
    let l_else  = new_label () in
    let l_merge = new_label () in

    (* branches *)
    let r_then, l_tend, b_t, bs_t = compile_llvm ethen env l_then [] in
    let r_else, l_eend, b_e, bs_e = compile_llvm eelse env l_else [] in

    (* blocks *)
    let blocks =
      bs0 @ [ (l0, b0 @ [BrI1 (rcond, l_then, l_else)]) ] @
      bs_t @ [ (l_tend, b_t @ [BrLabel l_merge]) ] @
      bs_e @ [ (l_eend, b_e @ [BrLabel l_merge]) ]
    in

    begin match t_if with
    | UnitT ->
        (* no value to merge, return dummy *)
        (Const 0, l_merge, [], blocks)
    | BoolT ->
        let r = new_reg () in
        (Register r, l_merge, [PhiI1  (r, [ (r_then, l_tend); (r_else, l_eend) ])], blocks)
    | IntT  ->
        let r = new_reg () in
        (Register r, l_merge, [PhiI32 (r, [ (r_then, l_tend); (r_else, l_eend) ])], blocks)
    | _ -> failwith "If error: returning unsupported type"
    end

  | PrintInt (_, e1) ->
    let v, l1, b1, bs1 = compile_llvm e1 env label block in
    let rcall = new_reg () in
    (Const 0, l1, b1 @ [CallPrintfI32 (rcall, v)], bs1)

  | PrintBool (_, e1) ->
    let v, l1, b1, bs1 = compile_llvm e1 env label block in
    let rz = new_reg () in
    let rcall = new_reg () in
    (Const 0, l1, b1 @ [ZextI1ToI32 (rz, v); CallPrintfI32 (rcall, Register rz)], bs1)

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

  | New (t, e1) ->
    let v, l1, b1, bs1 = compile_llvm e1 env label block in
    let r = new_reg () in
    begin match t with
    | RefT IntT -> (Ptr r,  l1, b1 @ [CallNew (IntK,  r, v)], bs1)
    | RefT BoolT -> (Ptr r,  l1, b1 @ [CallNew (BoolK, r, v)], bs1)
    | RefT (RefT _) -> (Ptr r,  l1, b1 @ [CallNew (RefK,  r, v)], bs1)
    | RefT _ | _ -> failwith "New error: unsupported element type"
    end

  | Deref (t, e1) ->
    let p, l1, b1, bs1 = compile_llvm e1 env label block in
    let r = new_reg () in
    begin match t with
    | IntT -> (Register r, l1, b1 @ [CallLoad (IntK,  r, p)], bs1)
    | BoolT -> (Register r, l1, b1 @ [CallLoad (BoolK, r, p)], bs1)
    | RefT _ -> (Ptr r, l1, b1 @ [CallLoad (RefK,  r, p)], bs1)
    | _ -> failwith "Deref error: unsupported element type"
    end

  | Assign (_, e1, e2) ->
    (* p is the pointer to the cell; v is the value to write *)
    let p, l1, b1, bs1 = compile_llvm e1 env label block in
    let v, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
    (* discover the contents of the ref on the left-hand side *)
    let lhs_t = Typing.type_of e1 in
    let instr =
      match lhs_t with
      | RefT IntT -> CallStore (IntK, p, v)
      | RefT BoolT -> CallStore (BoolK, p, v)
      | RefT (RefT _) -> CallStore (RefK, p, v)
      | _ -> failwith "Assign error: lhs must be ref(_)"
    in
    (Const 0, l2, b2 @ [instr], bs1 @ bs2)

  | Free (_, e1) ->
    let p, l1, b1, bs1 = compile_llvm e1 env label block in
    (Const 0, l1, b1 @ [CallFree p], bs1)

  | Seq (_, e1, e2) ->
    let _, l1, b1, bs1 = compile_llvm e1 env label block in
    let r2, l2, b2, bs2 = compile_llvm e2 env l1 b1 in
    (r2, l2, b2, bs1 @ bs2)

  | While (_, econd, ebody) ->
    let l_cond = new_label () in
    let l_body = new_label () in
    let l_exit = new_label () in
    (* jump to condition *)
    let blocks0 = [ (label, block @ [BrLabel l_cond]) ] in
    (* condition *)
    let rc, lc, bc, bsc = compile_llvm econd env l_cond [] in
    let blk_cond = (lc, bc @ [BrI1 (rc, l_body, l_exit)]) in
    (* body *)
    let _, lb, bb, bsb = compile_llvm ebody env l_body [] in
    let blk_body_end = (lb, bb @ [BrLabel l_cond]) in
    (Const 0, l_exit, [], blocks0 @ bsc @ [blk_cond] @ bsb @ [blk_body_end])

  | Fun (fun_ty, param_name, param_ty, body) ->
    (* fun_ty vem do typechecker, deve ser FunT(param_ty, ret_ty) *)
    let ret_ty =
      match fun_ty with
      | FunT (_, rt) -> rt
      | NoneT msg ->
          failwith ("internal: Fun has NoneT type: " ^ msg)
      | _ ->
          failwith "internal: Fun node with non-FunT type"
    in

    let fid = new_fun_id () in
    let fname = "fun" ^ string_of_int fid in
    let entry = new_label () in

    (* Create a new environment scope for the function and bind the parameter *)
    let env_fun = Env.begin_scope Env.empty_env in
    let env_fun = Env.bind env_fun param_name (Register 0) in

    let r_body, l_end, b_end, bs_end =
      compile_llvm body env_fun entry []
    in

    (* choose the correct RetI* based on the inferred ret_ty *)
    let ret_instrs =
      match ret_ty with
      | IntT  -> [RetI32 r_body]
      | BoolT -> [RetI1 r_body]
      | RefT _ -> [RetPtr r_body]
      | FunT _ -> [RetPtr r_body]
      | UnitT -> [RetVoid]
      | NoneT msg ->
          failwith ("internal: function body has type error: " ^ msg)
    in

    let func_blocks = bs_end @ [ (l_end, b_end @ ret_instrs) ] in

    functions := {
      fname;
      param_name;
      ret_type = ret_ty;    (* inferred return type, for bookkeeping only *)
      param_type = param_ty;
      blocks = func_blocks;
      entry_label = entry;
    } :: !functions;

    (* value of the Fun expression is a pointer to that function *)
    (FunPtr fname, label, block, [])

  | App (ret_ty, e_fun, e_arg) ->
    (* function type: FunT(param_ty, ret_ty_check) *)
    let fun_ty = Typing.type_of e_fun in
    let param_ty, ret_ty_check =
      match fun_ty with
      | FunT (pt, rt) -> (pt, rt)
      | NoneT msg ->
          failwith ("internal: App on ill-typed expression: " ^ msg)
      | _ ->
          failwith "internal: App on value that is not a function"
    in

    (* ensure consistency between App's ann and fun_ty *)
    let _ =
      match ret_ty, ret_ty_check with
      | NoneT _, _ -> ()
      | _, NoneT _ -> ()
      | _ when ret_ty <> ret_ty_check ->
          failwith "internal: return type in App does not match FunT"
      | _ -> ()
    in

    (* compile the function and the argument *)
    let r_fun, l1, b1, bs1 = compile_llvm e_fun env label block in
    let r_arg, l2, b2, bs2 = compile_llvm e_arg env l1 b1 in

    (* r_fun must be FunPtr function_name *)
    let fname =
      match r_fun with
      | FunPtr n -> n
      | _ -> failwith "internal: attempt to apply a value that is not a FunPtr"
    in

    (* generate the call according to the inferred return type ret_ty *)
    let res, call_instr =
      match ret_ty with
      | IntT ->
          let r = new_reg () in
          (Register r, CallFunI32 (r, fname, param_ty, r_arg))

      | BoolT ->
          let r = new_reg () in
          (Register r, CallFunI1 (r, fname, param_ty, r_arg))

      | UnitT ->
          (Const 0, CallFunVoid (fname, param_ty, r_arg))

      | RefT _ ->
          let r = new_reg () in
          (Ptr r, CallFunPtr (r, fname, param_ty, r_arg))

      | FunT _ ->
          let r = new_reg () in
          (Ptr r, CallFunPtr (r, fname, param_ty, r_arg))

      | NoneT msg ->
          failwith ("internal: App with type error: " ^ msg)
    in

    (res, l2, b2 @ [call_instr], bs1 @ bs2)


  | _ -> failwith "Unexpected llvm node in typing AST"




(* === Unparse / print helpers === *)

let prologue =
  ["@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1";
   "define i32 @main() #0 {"]

let epilogue =
  ["  ret i32 0";
   "}";
   "declare i32 @printf(i8* noundef, ...) #1";
   "declare ptr  @new_ref_int(i32 noundef)";
   "declare ptr  @new_ref_bool(i1 noundef)";
   "declare ptr  @new_ref_ref(ptr noundef)";
   "declare i32  @deref_int(ptr noundef)";
   "declare i1   @deref_bool(ptr noundef)";
   "declare ptr  @deref_ref(ptr noundef)";
   "declare void @assign_int(ptr noundef, i32 noundef)";
   "declare void @assign_bool(ptr noundef, i1 noundef)";
   "declare void @assign_ref(ptr noundef, ptr noundef)";
   "declare void @free_ref(ptr noundef)"]

let unparse_register n = "%" ^ string_of_int n
let unparse_label_use n = "%L" ^ string_of_int n
let unparse_label_declaration l = "L" ^ string_of_int l ^ ":"

let unparse_result = function
  | Const x -> string_of_int x
  | Register x -> unparse_register x
  | Ptr r -> unparse_register r
  | FunPtr name -> "@" ^ name

(* for calls that require an actual pointer and cannot accept other result types *)
let unparse_ptr = function
  | Ptr r -> unparse_register r
  | _ -> failwith "Unparse: expected pointer result"

let unparse_type = function
  | IntT -> "i32"
  | BoolT -> "i1"
  | _ -> failwith "Unparse: unknown type"

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
  | PhiI32 (r, l) ->
      "  " ^ unparse_register r ^ " = phi i32 " ^
      String.concat ", " (List.map (fun (r, l) -> "[" ^ unparse_result r ^ ", " ^ unparse_label_use l ^ "]") l)
  | ZextI1ToI32 (r, v) ->
      "  " ^ unparse_register r ^ " = zext i1 " ^ unparse_result v ^ " to i32"
  | CallPrintfI32 (r, v) ->
      "  " ^ unparse_register r ^ " = call i32 (i8*, ...) @printf(i8* noundef @.str, i32 noundef " ^
      unparse_result v ^ ")"
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

  | CallNew (k, r, v) ->
      let f_name, arg =
        match k with
        | IntK  -> ("@new_ref_int",  "i32 " ^ unparse_result v)
        | BoolK -> ("@new_ref_bool", "i1 "  ^ unparse_result v)
        | RefK  -> ("@new_ref_ref",  "ptr " ^ unparse_result v)
      in
      "  " ^ unparse_register r ^ " = call ptr " ^ f_name ^ "(" ^ arg ^ ")"
  | CallLoad (k, r, p) ->
      let f_name, ret_ty =
        match k with
        | IntK  -> ("@deref_int",  "i32")
        | BoolK -> ("@deref_bool", "i1")
        | RefK  -> ("@deref_ref",  "ptr")
      in
      "  " ^ unparse_register r ^ " = call " ^ ret_ty ^ " " ^ f_name ^ "(ptr noundef " ^ unparse_ptr p ^ ")"
  | CallStore (k, p, v) ->
      let f_name, v_arg =
        match k with
        | IntK  -> ("@assign_int",  "i32 " ^ unparse_result v)
        | BoolK -> ("@assign_bool", "i1 "  ^ unparse_result v)
        | RefK  -> ("@assign_ref",  "ptr " ^ unparse_result v)
      in
      "  call void " ^ f_name ^ "(ptr noundef " ^ unparse_ptr p ^ ", " ^ v_arg ^ ")"
  | CallFree p ->
      "  call void @free_ref(ptr noundef " ^ unparse_ptr p ^ ")"

  | CallFunI32 (r, fname, param_ty, arg) ->
      let ll_ty = llvm_type_of_param param_ty in
      "  " ^ unparse_register r ^
      " = call i32 @" ^ fname ^
      "(" ^ ll_ty ^ " " ^ unparse_result arg ^ ")"

  | CallFunI1 (r, fname, param_ty, arg) ->
      let ll_ty = llvm_type_of_param param_ty in
      "  " ^ unparse_register r ^
      " = call i1 @" ^ fname ^
      "(" ^ ll_ty ^ " " ^ unparse_result arg ^ ")"

  | CallFunPtr (r, fname, param_ty, arg) ->
      let ll_ty = llvm_type_of_param param_ty in
      "  " ^ unparse_register r ^
      " = call ptr @" ^ fname ^
      "(" ^ ll_ty ^ " " ^ unparse_result arg ^ ")"

  | CallFunVoid (fname, param_ty, arg) ->
      let ll_ty = llvm_type_of_param param_ty in
      "  call void @" ^ fname ^
      "(" ^ ll_ty ^ " " ^ unparse_result arg ^ ")"

  | RetI32 v ->
      "  ret i32 " ^ unparse_result v
  | RetI1 v ->
      "  ret i1 " ^ unparse_result v
  | RetPtr v ->
      "  ret ptr " ^ unparse_result v
  | RetVoid ->
      "  ret void"

  
  (* | _ -> failwith "Not implemented yet" *)

let print_block (label, instructions) =
  print_endline (unparse_label_declaration label);
  List.iter (fun x -> x |> unparse_llvm_i |> print_endline) instructions

let print_blocks bs = List.iter print_block bs

let emit_printf ret t =
  match t with
  | IntT ->
      "  " ^ unparse_register (new_reg ()) ^
      " = call i32 (i8*, ...) @printf(i8* noundef @.str, i32 noundef " ^
      unparse_result ret ^ ")"
  | BoolT ->
      let z = new_reg () in
      let zline =
        "  " ^ unparse_register z ^ " = zext i1 " ^ unparse_result ret ^ " to i32"
      in
      let call =
        "  " ^ unparse_register (new_reg ()) ^
        " = call i32 (i8*, ...) @printf(i8* noundef @.str, i32 noundef " ^
        unparse_register z ^ ")"
      in
      zline ^ "\n" ^ call
  | UnitT -> ""  (* nothing to print *)
  | _ -> failwith "Unsupported top-level result type for printf"

let print_llvm (ret, label, instructions, blocks) t =
  List.iter print_endline prologue;
  print_blocks (blocks @ [(label, instructions)]);
  let trailer = emit_printf ret t in
  if trailer <> "" then print_endline trailer;
  List.iter print_endline epilogue

let compile e = compile_llvm e [] 0 []
