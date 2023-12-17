%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> VAR
%token <string> CONST
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN 
%token ELSE
%token DO
%token WHILE
%token LPAREN
%token RPAREN
%token EOF

%left SEQ
%nonassoc ELSE DO


%left OR
%left AND
%left NOT
%nonassoc EQ LEQ
%left ADD SUB
%left MUL



%start <cmd> prog

%%

prog:
    | e = cmd; EOF { e }
;

expr:
    | TRUE {True}
    | FALSE {False}
    | c=CONST {Const(int_of_string c)}
    | NOT;e1=expr;{Not(e1)}
    | v=VAR;{Var(v)}
    | e1=expr;AND;e2=expr;{And(e1,e2)}
    | e1=expr;OR;e2=expr;{Or(e1,e2)}
    | e1=expr;ADD;e2=expr;{Add(e1,e2)}
    | e1=expr;SUB;e2=expr;{Sub(e1,e2)}
    | e1=expr;MUL;e2=expr;{Mul(e1,e2)}
    | e1=expr;EQ;e2=expr;{Eq(e1,e2)}
    | e1=expr;LEQ;e2=expr;{Leq(e1,e2)}
    | LPAREN e=expr RPAREN; {e}
;

cmd:
    | SKIP {Skip}
    | WHILE e1=expr; DO; c1=cmd; {While(e1,c1)}
    | IF e1=expr; THEN; c1=cmd; ELSE c2=cmd; {If(e1,c1,c2)}
    | i=VAR;ASSIGN;e=expr; {Assign(i,e)}
    | c1=cmd;SEQ;c2=cmd;{Seq(c1,c2)}
    | LPAREN; c=cmd; RPAREN; { c }
;
