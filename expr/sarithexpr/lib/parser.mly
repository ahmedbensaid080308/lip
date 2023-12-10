%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token ZERO 
%token SUCC
%token PRED
%token ISZERO
%token EOF

%left ELSE
%left OR
%left AND
%left NOT
%left ISZERO
%left SUCC PRED

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | ZERO {Zero}
  | TRUE { True }
  | FALSE { False }
  | NOT; e1=expr; {Not(e1)}
  | e1=expr; AND; e2=expr; {And(e1,e2)}
  | e1=expr; OR; e2=expr; {Or(e1,e2)}
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | ISZERO; e1 = expr; {IsZero(e1)}
  | PRED; e1=expr; {Pred(e1)}
  | SUCC; e1=expr; {Succ(e1)}
  | LPAREN; e=expr; RPAREN {e}
;

