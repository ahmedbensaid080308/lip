{
    open Parser


    exception Error
}

let white =[' ' '\t']+
let var = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let const = ['0'-'9']+

rule read =
    parse
    | white { read lexbuf }
    | "true" { TRUE }
    | "false" { FALSE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "and" {AND}
    | "or" {OR}
    | "not" {NOT}
    | "do" {DO}
    | "+" {ADD}
    | "-" {SUB}
    | "*" {MUL}
    | "<=" {LEQ}
    | ":=" {ASSIGN}
    | "=" {EQ}
    | "while" {WHILE}
    | "skip" {SKIP}
    | ";" {SEQ}
    | "int" {INT}
    | "bool" {BOOL}
    | "{" {LBRACE}
    | "}" {RBRACE}
    | const {CONST (Lexing.lexeme lexbuf)}
    | var {VAR (Lexing.lexeme lexbuf)}
    | eof { EOF }
    | _ {raise Error}