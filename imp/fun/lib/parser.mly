%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> IDE
%token INT
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
%token LBRACE
%token RBRACE
%token FUN
%token RETURN
%token EOF

%left SEQ
%nonassoc ELSE DO


%left OR
%left AND
%left NOT
%nonassoc EQ LEQ
%left ADD SUB
%left MUL



%start <prog> prog

%%

prog:
    | d=decl; e=cmd; EOF {Prog(d,e)}
;

expr:
    | TRUE {True}
    | FALSE {False}
    | c=CONST {Const(int_of_string c)}
    | NOT;e1=expr;{Not(e1)}
    | f=IDE;LPAREN;p=expr;RPAREN {Call(f,p)}
    | v=IDE; {Var(v)}
    | e1=expr;AND;e2=expr;{And(e1,e2)}
    | e1=expr;OR;e2=expr;{Or(e1,e2)}
    | e1=expr;ADD;e2=expr;{Add(e1,e2)}
    | e1=expr;SUB;e2=expr;{Sub(e1,e2)}
    | e1=expr;MUL;e2=expr;{Mul(e1,e2)}
    | e1=expr;EQ;e2=expr;{Eq(e1,e2)}
    | e1=expr;LEQ;e2=expr;{Leq(e1,e2)}
    | LPAREN e=expr RPAREN; {e}
;

decl:
    | {EmptyDecl}
    | INT; i=IDE; {IntVar(i)}
    | FUN; f=IDE; LPAREN; p=IDE; RPAREN; LBRACE; c=cmd; SEQ; RETURN; e=expr; RBRACE; {Fun(f,p,c,e)} 
    | d1=decl; SEQ; d2=decl; {DSeq(d1,d2)}

cmd:
    | SKIP {Skip}
    | WHILE e1=expr; DO; c1=cmd; {While(e1,c1)}
    | IF e1=expr; THEN; c1=cmd; ELSE c2=cmd; {If(e1,c1,c2)}
    | i=IDE;ASSIGN;e=expr; {Assign(i,e)}
    | c1=cmd;SEQ;c2=cmd;{Seq(c1,c2)}
    | LPAREN; c=cmd; RPAREN; { c }
;
