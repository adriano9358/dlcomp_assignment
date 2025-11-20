(* This is the main entry point for the CALC → LLVM compiler *)

open Calc_lib

(* Parse a lexing buffer and handle syntax errors *)
let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith
        (Printf.sprintf "Parse error at line %d, column %d"
           pos.pos_lnum col)

(* Parse a program from a string *)
let parse_string s =
  Lexing.from_string s |> parse_lexbuf

(* Read the entire stdin (supports multi-line input and file redirection) *)
let read_all_stdin () =
  let buf = Buffer.create 1024 in
  (try
     while true do
       let line = input_line stdin in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  (* Detect whether optimization was requested *)
  let optimize_enabled =
    Array.exists (fun s -> s = "-O" || s = "--opt") Sys.argv
  in

  (* Friendly instructions to the user *)
  prerr_endline
    "Insert a Calc program by using dune exec calcc < prog.calc";
  prerr_endline "Finish input with an empty file or Ctrl+D.";
  prerr_endline "";

  (* Read the whole program from stdin *)
  let s = read_all_stdin () in

  (* Do nothing if the input is empty *)
  if String.trim s = "" then
    ()
  else
    (try
       (* Parse the program *)
       let e = parse_string s in

       (* Typecheck to obtain the typed AST *)
       let typed = Typing.typecheck e Env.empty_env in

       (* Apply constant folding only if enabled *)
       let typed_opt =
         if optimize_enabled then Optimize.optimize typed else typed
       in

       (* Determine the type after optimization *)
       let t = Typing.type_of typed_opt in

       (* Show program and type on stderr, not in the LLVM file *)
       Printf.eprintf "%s : %s\n%!"
         (Ast.unparse_ast 0 e)
         (Typing.unparse_type t);

       (* Reject typing errors *)
       (match t with
       | NoneT msg -> failwith ("Typing error: " ^ msg)
       | _ -> ());

       (* Compile to LLVM IR and print it *)
       let result = Llvm.compile typed_opt in
       Llvm.print_llvm result t

     with Failure msg ->
       Printf.eprintf "Error: %s\n%!" msg)