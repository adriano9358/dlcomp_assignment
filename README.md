# Language and Compiler Design - NOVA FCT 2025


This repository contains the code for the assignment of the course **Language and Compiler Design**.  
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
