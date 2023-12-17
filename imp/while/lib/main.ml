open Types
open Ast

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast


let bool_val = function
  | Bool(b)->b
  | _->raise (TypeError("Bool expected"))

let nat_val = function
  | Nat(n)->n
  | _->raise (TypeError("Nat expected"))


let get_expr_type = function
  | Bool(_)-> "Bool"
  | Nat(_)->"Nat"

let rec eval_expr env = function
    True->Bool(true)
  | False->Bool(false)
  | Const(c)->Nat(c)
  | Var(v)-> env v
  | Not(b)->Bool(not (bool_val (eval_expr env b)))
  | And(a,b)->Bool((bool_val (eval_expr env a))&&(bool_val (eval_expr env b)))
  | Or(a,b)->Bool((bool_val (eval_expr env a))||(bool_val (eval_expr env b)))
  | Add(a,b)->Nat((nat_val (eval_expr env a))+(nat_val (eval_expr env b)))
  | Sub(a,b)->Nat((nat_val (eval_expr env a))-(nat_val (eval_expr env b)))
  | Mul(a,b)->Nat((nat_val (eval_expr env a))*(nat_val (eval_expr env b)))
  | Leq(a,b)->Bool((nat_val (eval_expr env a))<(nat_val (eval_expr env b))||
                  (nat_val (eval_expr env a))=(nat_val (eval_expr env b)))
  | Eq(a,b)->
    if(get_expr_type(eval_expr env a)<>get_expr_type(eval_expr env b))then 
      Bool(false)
    else
      if(get_expr_type(eval_expr env a)=="Bool")then 
        Bool((bool_val (eval_expr env a))=(bool_val (eval_expr env b)))
      else
        Bool((nat_val (eval_expr env a))=(nat_val (eval_expr env b)))

let get_state_from_conf = function
  St(s)->s
  |Cmd(_, s)->s

let get_command_from_conf = function
  St(_)->raise NoRuleApplies
  |Cmd(c, _)->c


let rec trace1 (con : conf) =
  let st=get_state_from_conf con in(*funzione che mappa le variabili*)
  let cm=get_command_from_conf con in(*prossimo comando da eseguire*)
  match cm with
    Assign(s,e)->
      let value=eval_expr st e in
      let nst var=if(var=s)then value else st var in
      (St(nst))
    |Skip-> (St(st))
    |Seq(c1,c2)-> (*In c1 ci potrebbero essere a loro volta delle sequenze si istruzioni*)
      (match trace1 (Cmd (c1, st)) with
          | St nst -> Cmd (c2, nst)
          | Cmd (c1', nst) -> Cmd (Seq (c1', c2), nst))
    |If(c,e1,e2)->
      if(get_expr_type(eval_expr st c)<>"Bool")then
        raise (TypeError("Expected Bool"))
      else
        if(bool_val (eval_expr st c))then
          (Cmd(e1, st))
        else
          (Cmd(e2, st))
    |While(c,e)->
      if(get_expr_type(eval_expr st c)<>"Bool")then
        raise (TypeError("Expected Bool"))
      else
        if(bool_val (eval_expr st c))then
          (Cmd(Seq(e,While(c,e)), st))
        else
          (St(st))


       

let trace n e =
  let rec trace_rec n cnf = try
    let e' = trace1 cnf in 
    if(n>0)then
      e'::(trace_rec (n-1) e')
    else [cnf] 
  with NoRuleApplies -> [cnf] in
  trace_rec n (Cmd(e, (fun s -> raise (UnboundVar ("variable "^s^" not defined")))))
;;



        
