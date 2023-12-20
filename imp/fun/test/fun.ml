open FunLib.Types       
open FunLib.Prettyprint
open FunLib.Main

(**********************************************************************
 trace test : (command, n_steps, location, expected value after n_steps)
 **********************************************************************)
(*
let rec expr_to_string = function
    True->"True"
    |False->"False"
    |Var(ide)->"Var("^ide^")"
    |Const(c)->"Const("^(string_of_int c)^")"
    |Not(e)->"Not("^(expr_to_string e)^")"
    |And(e1,e2)->"And("^(expr_to_string e1)^","^(expr_to_string e2)^")"
    |Or(e1,e2)->"Or("^(expr_to_string e1)^","^(expr_to_string e2)^")"
    |Add(n1,n2)->"Add("^(expr_to_string n1)^","^(expr_to_string n2)^")"
    |Sub(n1,n2)->"Add("^(expr_to_string n1)^","^(expr_to_string n2)^")"
    |Mul(n1,n2)->"Add("^(expr_to_string n1)^","^(expr_to_string n2)^")"
    |Eq(n1,n2)->"Eq("^(expr_to_string n1)^","^(expr_to_string n2)^")"
    |Leq(n1,n2)->"Leq("^(expr_to_string n1)^","^(expr_to_string n2)^")"
    |Call(f,e)->"Add("^f^","^(expr_to_string e)^")"
    |_->failwith"Errore"

let rec cmd_to_string = function
    Skip->"Skip"
    |Assign(s,e)->"Assign("^s^","^(expr_to_string e)^")"
    |Seq(cmd1,cmd2)->"Seq("^cmd_to_string cmd1^","^cmd_to_string cmd2^")"
    |If(e,cmd1,cmd2)->"If("^(expr_to_string e)^","^cmd_to_string cmd1^","^cmd_to_string cmd2^")"
    |While(e,cmd)->"While("^(expr_to_string e)^","^cmd_to_string cmd^")"
*)

let test_trace (cmd,n_steps,var,exp_val) =
  cmd
  |> parse
  |> fun c -> last (trace n_steps c)
  |> fun t -> match t with
    St s -> apply s var = exp_val
  | Cmd(_,_) -> failwith ("program not terminated")

let%test "test_trace1" = test_trace
  ("int x; x:=51", 2, "x", 51)  

let%test "test_trace2" = test_trace
    ("int x; x:=0; x:=x+1", 5, "x", 1)

let%test "test_trace3" = test_trace
    ("int x; int y; x:=0; y:=x+1; x:=y+1", 10, "x", 2)

let%test "test_trace4" = test_trace
    ("int x; int y; x:=0; if x=0 then y:=10 else y:=20", 5, "y", 10)

let%test "test_trace5" = test_trace
    ("int x; int y; x:=1; if x=0 then y:=10 else y:=20", 5, "y", 20)

let%test "test_trace6" = test_trace
    ("int x; int y; int r; x:=3; y:=2; r:=0; while 1<=y do ( r:=r+x; y:=y-1 )", 30, "r", 6)

let%test "test_trace7" = test_trace
    ("int x; int y; x:=3; while 0<=x and not 0=x do x:=x-1; x:=5", 50, "x", 5)

let%test "test_trace8" = test_trace
    ("int min; int x; int y; x:=5; y:=3; if x<=y then min:=x else min:=y", 40, "min", 3)

let%test "test_trace9" = test_trace
    ("int min; int x; int y; int z; x:=1; y:=2; z:=3; if x<=y and x<=z then min:=x else ( if y<=z then min:=y else min:=z )", 40, "min", 1)

let%test "test_trace10" = test_trace
    ("int x; fun f(y) { skip; return y+1 }; x := f(10)", 20, "x", 11)

let%test "test_trace11" = test_trace
    ("int x; fun f(y) { skip; return y+1 }; fun g(z) { skip; return f(z)+2 }; x := g(10)", 20, "x", 13)

let%test "test_trace12" = test_trace
    ("int x; int z; fun f(y) { x:=x+1; return x }; x := 10; z := f(0)", 20, "x", 11)

let%test "test_trace12" = test_trace
    ("int x; int z; fun f(y) { x:=x+1; return x }; x := 10; z := f(0)", 20, "z", 11)

let%test "test_trace13" = test_trace
    ("int x; int y; int w; fun f(z) { z:=x; x:=y; y:=z; return 0 }; x := 10; y := 20; w := f(0)", 20, "x", 20)

let%test "test_trace14" = test_trace
    ("int x; int y; int w; fun f(z) { z:=x; x:=y; y:=z; return 0 }; x := 10; y := 20; w := f(0)", 20, "y", 10)

let%test "test_trace15" = test_trace
    ("int x; int y; fun f(x) { x:=20; return 0 }; x := 10; y := f(0); x := x+1", 20, "x", 11)
