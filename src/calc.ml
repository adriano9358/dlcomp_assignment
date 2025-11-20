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

let read_program () =
  let buf = Buffer.create 256 in
  let rec aux first_line =
    try
      let line = read_line () in
      if line = "" && not first_line then
        (* empty line after at least one line: end of block *)
        Buffer.contents buf
      else begin
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        aux false
      end
    with End_of_file ->
      (* if we have not read anything yet, propagate EOF to exit the REPL;
         if there were already lines, return the block that has been written *)
      if first_line && Buffer.length buf = 0 then
        raise End_of_file
      else
        Buffer.contents buf
  in
  aux true

let () =
  print_endline "Insert an expression (finish with empty line). Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    try
      let src = read_program () in
      if String.trim src = "" then
        (* empty block: ask for another *)
        loop ()
      else
        (try
           let e = parse_string src in
           let type_env = Env.empty_env in
           let eval_env = Env.empty_env in
           let typed_ast = Typing.typecheck e type_env in
           let expr_type = Typing.type_of typed_ast in
           match expr_type with
           | Calc_types.NoneT msg ->
               Printf.eprintf "Typing error: %s\n%!" msg
           | _ ->
               let v = Eval.eval e eval_env in
               Printf.printf " = %s : %s\n%!"
                 (Eval.unparse_result v)
                 (Typing.unparse_type expr_type)
         with Failure msg ->
           Printf.eprintf "Error: %s\n%!" msg);
      loop ()
    with End_of_file ->
      print_endline "\nGoodbye!"
  in
  loop ()