open Ast
open Types       


let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

type declex = St of state | Dcl of decl * state
let decl state dec =
  let rec decl_rec s= function 
      EmptyDecl->St(s)
    | IntVar(i)->
      let top_env=topenv s in
      let tl_env=popenv s in
      St(
        (fun id->if(id=i)then
          IVar(getloc s)
        else 
          top_env id)::tl_env,
        (getmem s),
        (getloc s)+1
      )
    | Fun(f,p,c,e)->
      let top_env=topenv s in
      let tl_env=popenv s in
      St(
        (fun id->if(id=f)then
          IFun(p,c,e)
        else 
          top_env id)::tl_env,
        (getmem s),
        (getloc s)
      )
    | DSeq(d1,d2)->
        (match decl_rec s d1 with
          | St(s')-> decl_rec s' d2
          | Dcl(d1',s') -> decl_rec s' (DSeq(d1',d2)) )
  in
  match decl_rec state dec with 
      St(s)->s
    | Dcl(_,_)->failwith"Errore dichiarazioni non terminate"

let rec trace1_expr st = function
  | Var(i)->((match (topenv st) i with
       IVar(l)->Const(getmem st l)
      |_->failwith (i^" Is not a variable")
    ),st)
  | Not(True)->(False,st)
  | Not(False)->(False,st)
  | Not(e)->let (e',st') = trace1_expr st e in (Not(e'),st')
  | And(True,e2)->(e2,st)
  | And(False,_)->(False,st)
  | And(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (And(e1', e2),st')
  | Or(True,_)->(True,st)
  | Or(False,e2)->(e2,st)
  | Or(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Or(e1', e2),st')
  | Add(Const(n1),Const(n2))->(Const(n1+n2),st)
  | Add(Const(n1),e2)->let (e2',st') = trace1_expr st e2 in
    (Add(Const(n1),e2'),st')
  | Add(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Add(e1', e2),st')
  | Sub(Const(n1),Const(n2))->(Const(n1-n2),st)
  | Sub(Const(n1),e2)->let (e2',st') = trace1_expr st e2 in
    (Sub(Const(n1),e2'),st')
  | Sub(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Sub(e1', e2),st')
  | Mul(Const(n1),Const(n2))->(Const(n1*n2),st)
  | Mul(Const(n1),e2)->let (e2',st') = trace1_expr st e2 in
    (Mul(Const(n1),e2'),st')
  | Mul(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Mul(e1', e2),st')
  | Leq(Const(n1),Const(n2))->((if(n1<=n2)then True else False),st)
  | Leq(Const(n1),e2)->let (e2',st') = trace1_expr st e2 in
    (Leq(Const(n1),e2'),st')
  | Leq(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Leq(e1', e2),st')
  | Eq(Const(n1),Const(n2))->((if(n1=n2)then True else False),st)
  | Eq(Const(n1),e2)->let (e2',st') = trace1_expr st e2 in
    (Eq(Const(n1),e2'),st')
  | Eq(e1,e2)->let (e1',st') = trace1_expr st e1 in
    (Eq(e1', e2),st')
  | Call(i, Const(n))->(match (topenv st) i with
      IFun(p, c, e)->
        let n_top=fun id->(
          if(id=p)then 
            IVar(getloc st)
          else
            (topenv st) id
        )in
        let n_env=n_top::(getenv st)in
      
        let n_mem=(fun l->
          if(IVar(l)=(n_top p))then
            n
          else
            (getmem st) l
        )in 
        let n_loc=(getloc st)+1 in
        let nst=(n_env,n_mem,n_loc) in
        (CallExec(c,e),nst)
      | _->failwith (i^" Is not a function")
    )
  | Call(i, e)->let (e', st')=trace1_expr st e in (Call(i,e'),st')
  | CallExec(c, e)-> 
      (match trace1_cmd st c with
         Cmd(c',st')->(CallExec(c', e),st')  
        | St(st')->(CallRet(e),st'))    
  | CallRet(Const(c))->
    let n_env = popenv st in
    let st'=(n_env,getmem st, getloc st)in
    (Const(c),st')
  | CallRet(e)->let (e',st')= trace1_expr st e in (CallRet(e'),st') 
  | _->raise NoRuleApplies

and trace1_cmd st = function
  Skip->St(st)
  | Assign(i,Const(n))->
      let l = topenv st i in
      let n_mem = fun loc->(
        if(IVar(loc) = l)then
          n
        else 
          (getmem st loc) 
      )in
      let st' = (getenv st, n_mem, getloc st) in
      St(st')   
  | Assign(i,e)->
      let (e',st')=trace1_expr st e in
      Cmd(Assign(i,e'),st')
  | Seq(c1,c2)->
      (match trace1_cmd st c1 with
        |St(st')-> Cmd(c2,st')
        |Cmd(c1',st')->Cmd(Seq(c1',c2),st')
      )
  | If(True,c1,_)->Cmd(c1,st)
  | If(False,_,c2)->Cmd(c2,st)
  | If(e,c1,c2)->
      let (e',st') = trace1_expr st e in
      Cmd(If(e',c1,c2),st')
  | While(e,c)->Cmd(If(e,Seq(c,While(e,c)),Skip),st)     

let trace n_step prog =
  let st0 = ([(fun i-> raise (UnboundVar (i^" Is not defined")))],
                (fun l-> failwith ((string_of_int l)^" Variable not initialized")), 0) in
  let rec trace_rec conf n = try
    if(n=0)then 
      [conf]
    else
      match conf with
        Cmd(c,st)->
          let conf'= trace1_cmd st c in
          conf'::(trace_rec conf' (n-1))
        |_ -> raise NoRuleApplies
  with NoRuleApplies->[conf] in
  match prog with
    Prog(d,c)->
      let st1 = decl st0 d in
      trace_rec (Cmd(c,st1)) n_step 

let apply state var =
  let env = topenv state in
  let mem = getmem state in
  match env var with
    IVar(l)->mem l
    |_->failwith (var^" is not a variable")