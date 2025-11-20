(* This is the main entry point for the calc language interpreter *)

open Calc_lib


let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)

let parse_string s =
  Lexing.from_string s |> parse_lexbuf


let loop optimize_enabled =
  (* First the prompt *)
  print_string "> "; flush stdout;
  match read_line () with
  | s ->
      (try
        (* Parse the string and return an AST *)
          let e = parse_string s in 
          (* Print it out *)
          print_string (Ast.unparse_ast 0 e^" : "); flush stdout;
          let e' = Typing.typecheck e Env.empty_env in
          print_endline (Typing.unparse_type (Typing.type_of e'));
          let t = Typing.type_of e' in
          begin match t with 
           | NoneT m -> failwith ("Typing error: " ^ m)
           | _ ->
                  (* aplicar constant folding se o flag estiver ativo *)
                  let e_opt =
                    if optimize_enabled
                    then Optimize.optimize e'
                    else e'
                  in
                  (* compilar o programa (original ou otimizado) *)
                  let result = Llvm.compile e_opt in
                  Llvm.print_llvm result t
           end
        with Failure msg ->
          Printf.eprintf "Error: %s\n%!" msg);
          
  | exception End_of_file -> print_endline "\nGoodbye!"

(* You cannot have top level computations, so we use, let () = ... *)
let () =
  (* ver se o utilizador passou -O ou --opt na linha de comandos *)
  let optimize_enabled =
    Array.exists
      (fun s -> s = "-O" || s = "--opt")
      Sys.argv
  in
  print_endline "Insert an expression. Ctrl+D to exit.";
  loop optimize_enabled
 
