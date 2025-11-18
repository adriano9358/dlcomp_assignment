open Calc_lib

let test name input expected_output =
  print_string ("Testing: " ^ name ^ "... ");
  flush stdout;
  try
    let expr = Calc_lib.Parser.main Calc_lib.Lexer.read (Lexing.from_string input) in
    let type_env = Env.empty_env in
    let eval_env = Env.empty_env in
    let _ = Typing.typecheck expr type_env in
    let result = Eval.eval expr eval_env in
    
    if result = expected_output then
      print_endline "PASS"
    else
      print_endline ("FAIL - expected: " ^ (Eval.unparse_result expected_output) ^ 
                    ", got: " ^ (Eval.unparse_result result))
  with
  | e -> 
      print_endline ("FAIL - exception: " ^ Printexc.to_string e)

let run_tests () =
  print_endline "Running Calc Language Tests...";
  print_endline "================================";
  
  (* Arithmetic tests *)
  test "Addition" "2 + 3" (Eval.IntV 5);
  test "Multiplication" "3 * 4" (Eval.IntV 12);
  test "Complex expression" "2 + 3 * 4" (Eval.IntV 14);
  test "Parentheses" "(2 + 3) * 4" (Eval.IntV 20);
  
  (* Boolean tests *)
  test "Boolean AND" "true && false" (Eval.BoolV false);
  test "Boolean OR" "true || false" (Eval.BoolV true);
  test "Boolean NOT" "not true" (Eval.BoolV false);
  
  (* Variable tests *)
  test "Let binding" "let x = 5 in x + 3" (Eval.IntV 8);
  test "Nested let" "let x = 2 in let y = 3 in x * y" (Eval.IntV 6);
  
  (* Conditional tests *)
  test "If true" "if true then 1 else 0" (Eval.IntV 1);
  test "If false" "if false then 1 else 0" (Eval.IntV 0);
  
  (* Function tests - if your language supports them *)
  test "Simple function" "let f = fun x : int -> x + 1 in f(5)" (Eval.IntV 6);
  test "Function with boolean" "let f = fun b : bool -> if b then 1 else 0 in f(true)" (Eval.IntV 1);
  test "Function with ref type" "let r = new(10) in let f = fun x : ref(int) -> !x + 5 in f(r)" (Eval.IntV 15);
  
  print_endline "================================";
  print_endline "Tests completed."

let () = run_tests ()