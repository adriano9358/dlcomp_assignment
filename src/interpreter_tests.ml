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
      print_endline " PASS"
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
  test "Addition"                   "2 + 3"         (Eval.IntV 5);
  test "Subtraction"                "5 - 2"         (Eval.IntV 3);
  test "Negation"                   "-4"            (Eval.IntV (-4));
  test "Division"                   "8 / 2"         (Eval.IntV 4);
  test "Division with remainder"    "7 / 2"         (Eval.IntV 3);
  test "Multiplication"             "3 * 4"         (Eval.IntV 12);
  test "Complex expression"         "2 + 3 * 4"     (Eval.IntV 14);
  test "Parentheses"                "(2 + 3) * 4"   (Eval.IntV 20);
  
  (* Boolean tests *)
  test "Boolean AND"  "true && false" (Eval.BoolV false);
  test "Boolean OR"   "true || false" (Eval.BoolV true);
  test "Boolean NOT"  "not true"      (Eval.BoolV false);

  (* Comparison tests *)
  test "Equality true"                "5 = 5"   (Eval.BoolV true);
  test "Equality false"               "5 = 3"   (Eval.BoolV false);
  test "Inequality true"              "5 != 3"  (Eval.BoolV true);
  test "Inequality false"             "5 != 5"  (Eval.BoolV false);
  test "Less than true"               "3 < 5"   (Eval.BoolV true);
  test "Less than false"              "5 < 3"   (Eval.BoolV false);
  test "Less than or equal true"      "5 <= 5"  (Eval.BoolV true);
  test "Less than or equal false"     "6 <= 5"  (Eval.BoolV false);
  test "Greater than true"            "5 > 3"   (Eval.BoolV true);
  test "Greater than false"           "3 > 5"   (Eval.BoolV false);
  test "Greater than or equal true"   "5 >= 5"  (Eval.BoolV true);
  test "Greater than or equal false"  "4 >= 5"  (Eval.BoolV false);
  
  (* Variable tests *)
  test "Let binding" "let x = 5 in x + 3" (Eval.IntV 8);
  test "Let with multiple bindings" "let x = 2 y = 3 in x + y" (Eval.IntV 5);
  test "Nested let" "let x = 2 in let y = 3 in x * y" (Eval.IntV 6);

  (* Sequence tests *)
  test "Sequence of expressions" "let x = new(5) in x := !x + 2 ; !x" (Eval.IntV 7);

  (* Reference tests *)
  test "Create reference" "let r = new(10) in !r" (Eval.IntV 10);
  test "Assign reference" "let r = new(5) in r := 15 ; !r" (Eval.IntV 15);
  
  (* Conditional tests *)
  test "If true" "if true then 1 else 0 end" (Eval.IntV 1);
  test "If false" "if false then 1 else 0 end" (Eval.IntV 0);

  (* While loop tests *)
  test "While loop" "let x = new(0) i = new(0) in while !i < 5 do x := !x + 1 ; i := !i + 1 end ; !x" (Eval.IntV 5);
  
  (* Function tests *)
  test "Simple function" "let f = fun x : int -> x + 1 in f(5)" (Eval.IntV 6);
  test "Function with boolean parameter" "let f = fun b : bool -> if b then 1 else 0 end in f(true)" (Eval.IntV 1);
  test "Function with ref type parameter" "let r = new(10) in let f = fun x : ref(int) -> !x + 5 in f(r)" (Eval.IntV 15);
  test "Function with function parameter" "let apply = fun f : (int -> int) -> f(2)+2 in apply(fun x : int -> x * 3)" (Eval.IntV 8);
  test "Nested functions" "let add = fun x : int -> fun y : int -> x + y in let addfive = add(5) in addfive(3)" (Eval.IntV 8);

  (* Print tests *)
  test "Print integer" "printInt(42)" (Eval.UnitV);
  test "Print boolean" "printBool(true)" (Eval.UnitV);
  (* test "Print endline" "printEndline()" (Eval.UnitV); *)

  (* Combined feature tests *)
  test "Most features combined" "
    let 
      x = 10 
      y = true
      inc = fun z : int -> z + 1
      applytwice = fun f : (int -> int) -> fun x : int -> f (f (x))
    in

    let 
      r = new(x * 2)
    in

    printInt( inc(5) ) ;
    printEndline() ;

    r := !r + 3 ;

    printInt( applytwice(inc)(!r) ) ;
    printEndline() ;

    while (!r < 30) do
      r := !r + 4
    end ;

    printBool( !r > 20 ) ;
    printEndline() ;

    free(r) ;

    if y && true then
      printInt( 999 )
    else
      printInt( 111 )
    end
  " (Eval.UnitV);

  test "Sum from 1 to n" "
    let 
      n = 10
      sum = new(0)
      i   = new(1)
    in

    while (!i <= n) do
      sum := !sum + !i ;
      i := !i + 1
    end ;

    printInt(!sum) ;

    free(sum) ;
    free(i)" (Eval.UnitV);

  print_endline "================================";
  print_endline "Tests completed."

let () = run_tests ()