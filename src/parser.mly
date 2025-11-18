%{
open Ast
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV LPAREN RPAREN AND OR NOT EQ NEQ LT LE GT GE TRUE FALSE EOF LET IN IF THEN ELSE WHILE DO NEW FREE PRINTINT PRINTBOOL PRINTENDLINE BANG ASSIGN SEMI END COLON ARROW INT_TYPE BOOL_TYPE UNIT_TYPE REF_TYPE FUN

%start main
%type <Ast.ast> main
%type <Ast.calc_typee> type_expr


%%
main:
  expr EOF                { $1 }

type_expr:
    | INT_TYPE                  { IntT }
    | BOOL_TYPE                 { BoolT }
    | REF_TYPE LPAREN type_expr  RPAREN      { RefT ($3) } 
    | UNIT_TYPE                 { UnitT }
    | type_expr ARROW type_expr { FunT($1, $3) }


expr:
  | expr SEMI expr          { Seq($1, $3) }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | WHILE expr DO expr END     { While($2, $4) }
  | assign_expr             { $1 }

  | LET bindings IN expr     { Let($2, $4) }

  | FUN ID COLON type_expr ARROW expr        { Fun($2, $4, $6) }
  


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
    | factor LPAREN expr RPAREN   { App($1,$3) }
    | atom  {$1}

atom:
  | NEW LPAREN expr RPAREN          { New($3) }
  | FREE LPAREN expr RPAREN         { Free($3) }
  | PRINTINT LPAREN expr RPAREN    { PrintInt($3) }
  | PRINTBOOL LPAREN expr RPAREN   { PrintBool($3) }
  | PRINTENDLINE LPAREN RPAREN      { PrintEndline }
  | BANG atom                     { Deref($2) }

  | TRUE                  { Bool true }
  | FALSE                 { Bool false }
  | INT                   { Num $1 }
  | LPAREN expr RPAREN    { $2 }
  | MINUS atom          { Neg $2 }
  | NOT atom           { Not $2 }
  | ID                    { Id $1 }

// factor: 
//   | expr LPAREN expr RPAREN       { App($1, $3) }

//   | NEW LPAREN expr RPAREN          { New($3) }
//   | FREE LPAREN expr RPAREN         { Free($3) }
//   | PRINTINT LPAREN expr RPAREN    { PrintInt($3) }
//   | PRINTBOOL LPAREN expr RPAREN   { PrintBool($3) }
//   | PRINTENDLINE LPAREN RPAREN      { PrintEndline }
//   | BANG factor                     { Deref($2) }

//   | TRUE                  { Bool true }
//   | FALSE                 { Bool false }
//   | INT                   { Num $1 }
//   | LPAREN expr RPAREN    { $2 }
//   | MINUS factor          { Neg $2 }
//   | NOT factor           { Not $2 }
//   | ID                    { Id $1 }
;