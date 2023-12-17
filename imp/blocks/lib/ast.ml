type ide = string
  
type expr =
  | True
  | False
  | Var of ide
  | Const of int     
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr

type decl =
  | EmptyDecl
  | IntVar of ide * decl
  | BoolVar of ide * decl

type cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Decl of decl * cmd (*quindi il parser genera solo decl e non block
     quando viene trovato un decl, vengono aggiunte le variabili dichiarate nello stato
     poi viene creato un block con i comandi del decl*)
  | Block of cmd (*quando si trova un block si pusha nello stack
     il suo stato, quando vinisce si fa il pop*)
  
