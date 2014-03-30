(* The representation of MOCaml patterns.

  p ::= intconst | boolconst | _ | var | [] | p::p
   
*)
type mopat =
    IntPat of int
  | BoolPat of bool
  | WildcardPat
  | VarPat of string
  | NilPat
  | ConsPat of mopat * mopat
;;

(* The representation of MOCaml expressions.

op ::= + | - | * | = | > | ::   
   
e ::= intconst | boolconst | [] | var | e1 op e2 | if e1 then e2 else e3
       | function p -> e | e1 e2
       | match e with p1 -> e1 '|' ... '|' pn -> en

MOCaml also supports the syntax [e1;...;en] for creating lists, but it
is converted by the parser into the expression e1::e2::...::en::[].
*)
type moop = Plus | Minus | Times | Eq | Gt | Cons
type moexpr =
    IntConst of int
  | BoolConst of bool
  | Nil
  | Var of string
  | BinOp of moexpr * moop * moexpr
  | If of moexpr * moexpr * moexpr
  | Function of mopat * moexpr
  | FunctionCall of moexpr * moexpr
  | Match of moexpr * (mopat * moexpr) list ;;

(* The representation of Mocaml declarations.

d ::= e | let x = e | let rec f p = e
   
*)

type modecl =
  | Expr of moexpr
  | Let of string * moexpr
  | LetRec of string * mopat * moexpr


(* The representation of MOCaml values, which are the results of
evaluating expressions.

  lv ::= [] | v::lv
  v ::= intconst | boolconst | function p -> e | lv
*)
type molistval =
    NilVal
  | ConsVal of movalue * molistval
and movalue =
    IntVal of int
  | BoolVal of bool
      (* A function value carries its lexical environment with it! *)
      (* If the function is recursive it also carries around its own name. *)
  | FunctionVal of string option * mopat * moexpr * moenv
  | ListVal of molistval      

(* The representation of value environments, which map strings to
values.  See env.ml for the definition of the Env.env type and
associated operations. *)
and moenv = movalue Env.env

   
(* The result from evaluating a declaration -- an optional name that
was declared along with the value of the right-hand-side
expression. *)
type moresult = string option * movalue
