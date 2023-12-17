
open Ast
open Types       


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast


let bool_val = function
  | Bool(b)->b
  | _->raise (TypeError("Bool expected"))

let int_val = function
  | Int(n)->n
  | _->raise (TypeError("Int expected"))

let get_loc_val = function
  | BVar(b)->b
  | IVar(i)->i


let get_expr_type = function
  | Bool(_)-> "Bool"
  | Int(_)->"Int"

let get_loc_type = function
  | BVar(_)-> "Bool"
  | IVar(_)-> "Int"

let update_env dec st =
  let rec helper dec' env loc=
    match dec' with
        EmptyDecl->(env,loc)
      | IntVar(ide, d)-> helper d (fun ide'->
          if(ide'=ide)then
            IVar(loc)
          else
            env ide'
        ) (loc+1)
      | BoolVar(ide, d)-> helper d (fun ide'->
          if(ide'=ide)then
            BVar(loc)
          else
            env ide'
      ) (loc+1)
  in

  let (env, loc)= helper dec (topenv st) (getloc st) in
  let nst = (env::(getenv st), getmem st, loc)in
  nst


let rec eval_expr env mem = function
    True->Bool(true)
  | False->Bool(false)
  | Const(c)->Int(c)
  | Var(v)-> (match env v with 
          BVar(b)->mem b
        | IVar(i)->mem i)
  | Not(b)->Bool(not (bool_val (eval_expr env mem b)))
  | And(a,b)->Bool((bool_val (eval_expr env mem a))&&(bool_val (eval_expr env mem b)))
  | Or(a,b)->Bool((bool_val (eval_expr env mem a))||(bool_val (eval_expr env mem b)))
  | Add(a,b)->Int((int_val (eval_expr env mem a))+(int_val (eval_expr env mem b)))
  | Sub(a,b)->Int((int_val (eval_expr env mem a))-(int_val (eval_expr env mem b)))
  | Mul(a,b)->Int((int_val (eval_expr env mem a))*(int_val (eval_expr env mem b)))
  | Leq(a,b)->Bool((int_val (eval_expr env mem a))<(int_val (eval_expr env mem b))||
                  (int_val (eval_expr env mem a))=(int_val (eval_expr env mem b)))
  | Eq(a,b)->
    if(get_expr_type(eval_expr env mem a)<>get_expr_type(eval_expr env mem b))then 
      Bool(false)
    else
      if(get_expr_type(eval_expr env mem a)=="Bool")then 
        Bool((bool_val (eval_expr env mem a))=(bool_val (eval_expr env mem b)))
      else
        Bool((int_val (eval_expr env mem a))=(int_val (eval_expr env mem b)))

let get_state_from_conf = function
  St(s)->s
  |Cmd(_, s)->s

let get_command_from_conf = function
  St(_)->raise NoRuleApplies
  |Cmd(c, _)->c

let rec list_len l = match l with
  []->0
  |_::l'->1+(list_len l')


let rec trace1 (con : conf) =
  let st=get_state_from_conf con in(*funzione che mappa le variabili*)
  let cm=get_command_from_conf con in(*prossimo comando da eseguire*)
  match cm with
    Assign(s,e)->
      let value=eval_expr (topenv st) (getmem st) e in
      let loc_s=(topenv st) s in

      if(get_expr_type value <> get_loc_type loc_s)then
        raise (TypeError ("Expected "^(get_loc_type loc_s)^" but "^(get_expr_type value)^" was given"))
      else 
        let nst=(getenv st, (fun l-> if(l=get_loc_val loc_s)then value else (getmem st) l), getloc st) in
        (St(nst))
      
    |Skip-> (St(st))
    |Seq(c1,c2)-> (*In c1 ci potrebbero essere a loro volta delle sequenze si istruzioni*)
      (match trace1 (Cmd (c1, st)) with
          | St nst -> Cmd (c2, nst)
          | Cmd (c1', nst) -> Cmd (Seq (c1', c2), nst))
    |If(c,e1,e2)->
      if(get_expr_type(eval_expr (topenv st) (getmem st) c)<>"Bool")then
        raise (TypeError("Expected Bool"))
      else
        if(bool_val (eval_expr (topenv st) (getmem st) c))then
          (Cmd(e1, st))
        else
          (Cmd(e2, st))
    |While(c,e)->
      if(get_expr_type(eval_expr (topenv st) (getmem st) c)<>"Bool")then
        raise (TypeError("Expected Bool"))
      else
        if(bool_val (eval_expr (topenv st) (getmem st) c))then
          (Cmd(Seq(e,While(c,e)), st))
        else
          (St(st))
    |Decl(d, c)->
      Cmd(Block(c),(update_env d st))
    |Block(c)->match trace1 (Cmd(c,st)) with
      | St st'-> St(popenv st', getmem st', getloc st')
      | Cmd (c', st')->Cmd(Block c', st')
       

let trace n e =
  let st0=([(fun s -> raise (UnboundVar ("variable "^s^" not defined")))],
            (fun l -> raise (NotInitialized ("variable "^(string_of_int l)^" not initialized"))), 0)in
  let rec trace_rec n cnf = try
    let e' = trace1 cnf in 
    if(n>0)then
      e'::(trace_rec (n-1) e')
    else [cnf] 
  with NoRuleApplies -> [cnf] in
  trace_rec n (Cmd(e, st0))
;;


        
