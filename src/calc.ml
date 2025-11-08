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

let () =
  print_endline "Insert an expression. Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    match read_line () with
    | s ->
        (try
           let e = parse_string s in 
           print_string (Ast.unparse_ast 100 e^" "); flush stdout;
           
           let type_env = Env.empty_env in
           let eval_env = Env.empty_env in
           
           let typed_ast = Typing.typecheck e type_env in
           let expr_type = Typing.type_of typed_ast in
           
           (match expr_type with
            | Typing.NoneT msg ->
                Printf.eprintf "Typing error: %s\n%!" msg
            | _ ->
                let v = Eval.eval e eval_env in
                Printf.printf " = %s : %s\n%!"
                  (Eval.unparse_result v)
                  (Typing.unparse_type expr_type))
         with Failure msg ->
           Printf.eprintf "Error: %s\n%!" msg);
        loop ()
    | exception End_of_file -> print_endline "\nGoodbye!"
  in
  loop ()
