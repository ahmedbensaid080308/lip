open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Zero -> "0"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e)->"Not("^(string_of_boolexpr e)^")"
  | Or(e0,e1)->"Or("^(string_of_boolexpr e0)^","^(string_of_boolexpr e1)^")"
  | And(e0,e1)->"And("^(string_of_boolexpr e0)^","^(string_of_boolexpr e1)^")"
  | Succ(e)-> "Succ("^ string_of_boolexpr e ^")"
  | Pred(e)-> "Pred("^ string_of_boolexpr e ^")"
  | IsZero(e)-> "IsZero("^ string_of_boolexpr e ^")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

type result= Nat of int | Bool of bool;;
let get_value_bool = function
  Bool(v)->v
  |_->failwith "error"
;;

let get_value_nat = function
  Nat(v)->v
  |_->failwith "error"
;;

let rec is_nv = function 
  Zero -> true
  |Succ(e)->(is_nv e)
  |_->false

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True)->False
  | Not(False)->True
  | Not(e)->let e' = trace1 e in Not(e')
  | And(True, e2)->e2
  | And(False, _)->False
  | And(e1,e2)-> let e1'=trace1 e1 in And(e1',e2)
  | Or(True, _)->True
  | Or(False, e2)->e2
  | Or(e1,e2)-> let e1'=trace1 e1 in Or(e1',e2)
  | Succ(e)->let e'=trace1 e in Succ(e')
  | Pred(Succ(e)) when(is_nv e)->e
  | Pred(e)->let e'=trace1 e in Pred(e')
  | IsZero(Zero)->True
  | IsZero(Succ(e)) when(is_nv e)->False
  | IsZero(e)->let e'=trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval = function
    True -> Bool(true)
  | False -> Bool(false)
  | Not(e)->Bool(not (get_value_bool(eval e)))
  | And(e1,e2)-> Bool((get_value_bool(eval e1)) && (get_value_bool(eval e2)))
  | Or(e1, e2)-> Bool((get_value_bool(eval e1)) || (get_value_bool(eval e2)))
  | If(e0,e1,e2) -> Bool(if (get_value_bool(eval e0)) then (get_value_bool(eval e1)) else (get_value_bool(eval e2)) )
  | Zero -> Nat(0)
  | Succ(e)-> Nat((get_value_nat(eval e))+1) 
  | Pred(e) when ((get_value_nat(eval e))>0) -> Nat((get_value_nat(eval e))-1)
  | IsZero(e)->Bool(if((get_value_nat(eval e))==0)then true else false)
  |_->failwith "error"
;;
