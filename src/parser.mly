%{
open Ast
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV LPAREN RPAREN AND OR NOT EQ NEQ LT LE GT GE TRUE FALSE EOF LET IN IF THEN ELSE WHILE DO NEW FREE PRINTINT PRINTBOOL PRINTENDLINE BANG ASSIGN SEMI END

%start main
%type <Ast.ast> main

%%
main:
  expr EOF                { $1 }

expr:
  | expr SEMI expr          { Seq($1, $3) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | WHILE expr DO expr END     { While($2, $4) }
  | assign_expr             { $1 }

  | LET bindings IN expr     { Let($2, $4) }
  


assign_expr:
  | disj ASSIGN assign_expr { Assign($1, $3) }
  | disj                    { $1 }

bindings:
  | ID EQ expr               { [($1, $3)] }
  | ID EQ expr bindings      { ($1, $3) :: $4 }

disj:
  | disj OR conj         { Or($1,$3)}
  | conj                  { $1 }  

conj:
  | conj AND comp         { And($1,$3)}
  | comp                  { $1 }  

comp:
  | comp EQ arith         { Eq($1, $3)}
  | comp NEQ arith        { Neq($1, $3) }
  | comp LT arith         { Lt($1, $3) }
  | comp LE arith         { Le($1, $3) }
  | comp GT arith         { Gt($1, $3) }
  | comp GE arith         { Ge($1, $3) }
  | arith                  {$1}

arith:
  | arith PLUS  term      { Add ($1, $3) }
  | arith MINUS term      { Sub ($1, $3) }
  | term                  { $1 }

term:
  | term TIMES  factor     { Mul ($1, $3) }
  | term DIV    factor     { Div ($1, $3) }
  | factor                 {$1}

factor: 
  | NEW LPAREN expr RPAREN          { New($3) }
  | FREE LPAREN expr RPAREN         { Free($3) }
  | PRINTINT LPAREN expr RPAREN    { PrintInt($3) }
  | PRINTBOOL LPAREN expr RPAREN   { PrintBool($3) }
  | PRINTENDLINE LPAREN RPAREN      { PrintEndline }
  | BANG factor                     { Deref($2) }

  | TRUE                  { Bool true }
  | FALSE                 { Bool false }
  | INT                   { Num $1 }
  | LPAREN expr RPAREN    { $2 }
  | MINUS factor          { Neg $2 }
  | NOT factor           { Not $2 }
  | ID                    { Id $1 }
;