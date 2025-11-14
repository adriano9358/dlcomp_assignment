{
  open Parser
  exception Lexing_error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n']     { read lexbuf }       (* skip whitespace *)
  | ['0'-'9']+ as i          { INT (int_of_string i) }
  | "fun"                   { FUN }
  | "->"                    { ARROW }
  | "let"                   { LET }
  | "in"                    { IN }
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }
  | "while"                 { WHILE }
  | "do"                    { DO }
  | "new"                   { NEW }
  | "free"                  { FREE }
  | "printInt"              { PRINTINT }
  | "printBool"             { PRINTBOOL }
  | "true"                  { TRUE }
  | "false"                 { FALSE }
  | "!"                     { BANG }
  | ":="                    { ASSIGN }
  | ';'                     { SEMI }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "not"                   { NOT }
  | "="                     { EQ }
  | "!="                    { NEQ }
  | "<"                     { LT }
  | ">"                     { GT }
  | "<="                    { LE }
  | ">="                    { GE }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIV }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | ['a'-'z']+ as id          { ID id }
  | eof                      { EOF }
  | _ as c                   { raise (Lexing_error (Printf.sprintf "Unexpected char: %c" c)) }