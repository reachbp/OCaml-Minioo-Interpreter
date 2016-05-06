(* File miniooLEX.mll *)
{
open MiniooYACC;; (* Type token defined in MiniooYACC.mli *)
exception Eof;;
}
rule token = parse
    [' ' '\t'] { token lexbuf } (* skip blanks and tabs *)
  | ['\n' ]    { EOL }
  | "var"   { VARIABLE }
  | "null"  {NULL}
  | "proc"  {PROC}
  | "malloc" {MALLOC}
  | "atom" {ATOM}
  | "if"  {IF}
  | "else"  {ELSE}
  | "while"  {WHILE}
  | "skip"  {SKIP}
  | "|||"     {PARALLEL}
  | ';'     {SEMICOLON}
  | ':'     {COLON}
  | (['0'-'9']+) as num       {NUMBER (int_of_string num)}
  | "true" {TRUE}
  | "false" {FALSE}
  | '='    { ASSIGN }
  | '<'    { LT }
  | "=="  { EQUALS }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { MULT }
  | '.'        { TIMES }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LCURL }
  | '}' { RCURL }
  | '/'        { DIV }
  | ['A'-'Z' ](['a'-'z']|['A'-'Z'])* as x  { FIELD x }
  | ['a'-'z' ](['a'-'z']|['A'-'Z'])* as f  { VAR f}
  | eof        { raise Eof }
