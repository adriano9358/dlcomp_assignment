# Language and Compiler Design - NOVA FCT 2025

## Lab 3 – `CALCB` definitional interpreter and compiler

This repository contains the starter code for **Lab 3** of the course **Language and Compiler Design**.  
In this lab, you will extend the previous lab sessions' work on the `CALCB` language. You need to add boolean constants, logical operators, and comparison operators to the language. You will also implement a compiler that translates `CALCB` expressions into LLVM code.

---

### Building the Project

Building the project follows the same steps as in previous labs. Make sure you have `dune` installed.

Run the following command inside the project root:

```bash
dune build
```

This compiles the interpreter and related modules.

### Running the Compiler

After building, you can run the interpreter with:

```bash
dune exec calcc
```

This will start the program that evaluates expressions written in the defined expression language.

The program asks for an expression to compile and outputs some LLVM code. For instance, if the expression is `1+2+3`, the output will be

```LLVM
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
define i32 @main() #0 {
%1 = add nsw i32 1, 2
%2 = add nsw i32 %1, 3
  %3 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %2)
  ret i32 0
}
declare i32 @printf(ptr noundef, ...) #1
```

This output should be placed in a file with the extension `ll` and compiled using `clang`

```bash
clang -o a a.ll
```

The result is the executable `a` which can be run from the console 

```bash
./a
```

to obtain the result `6`

### Extension points

1. Add boolean constants (`true` and `false`) to the syntax of the `CALCB` language (lexer and parser).

2. Add logical operators (`&&`, `||`, `not`) to the syntax of the `CALCB` language (lexer and parser). Pay special attention to operator precedence and associativity.

3. Add comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) to the syntax of the `CALCB` language (lexer and parser). Pay special attention to operator precedence and associativity.

4. Extend the interpreter to evaluate boolean constants, logical operators, and comparison operators. Notice that now the result of evaluating and expression is no longer an integer but can also be a boolean. Use a sum type to represent the result of evaluation.

```ocaml
type result = 
  | IntV of int
  | BoolV of bool
```

  you will need to also add a function that converts a `result` to a string for printing.

5. Modify the interpreter to account for boolean values and boolean operations.

6. Test the unparser and interpreter with expressions that use boolean constants, logical operators, and comparison operators.

7. Extend the compiler to generate LLVM code for boolean constants, logical operators, and comparison operators. 

  - Boolean values can be represented as `i1` in LLVM (1 bit integer).
  - Logical operations can be implemented using LLVM instructions such as `and`, `or`, and `xor`. Notice that we are not yet implementing short-circuit evaluation.
  - Comparison operations can be implemented using LLVM instructions such as `icmp`.

  Research LLVM instructions by reading the documentation and by compiling sample C programs to LLVM. The command to do so is

  ```bash
  clang -S -emit-llvm -c a.c -o a.ll
  ```

  the result will be a file called `a.ll`

