
(* Name: Roger Chen

   UID: 504043927

   Others With Whom I Discussed Things: Kailin Chang, Neil Marion, Andy Shih

   Other Resources I Consulted:
   Piazza
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i = j -> Env.empty_env()
      | (BoolPat(b), BoolVal(bv)) when b = bv -> Env.empty_env()
      | (WildcardPat, _) -> Env.empty_env()
      | (VarPat(s), x) -> Env.add_binding s x (Env.empty_env())
      | (NilPat, ListVal(NilVal)) -> Env.empty_env()
      | (ConsPat(e1, e2), ListVal(ConsVal(cv, lv))) -> Env.combine_envs (patMatch e1 cv) (patMatch e2 (ListVal lv))
      (*Combine the left and right environments for cons in order to match on both sides of the :: operator*)
      | _ -> raise MatchFailure

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i) -> IntVal(i)
      | BoolConst(b) -> BoolVal(b)
      | Nil -> ListVal(NilVal)
      | Var(s) -> (try (Env.lookup s env) with
                  Env.NotBound -> raise DynamicTypeError)    
      (* If the lookup raises an exception, then the variable doesn't exist, so raise DynamicTypeError *)
      | BinOp(e1, op, e2) -> (match ((evalExpr e1 env), op, (evalExpr e2 env)) with
                             (IntVal(x), Plus, IntVal(y)) -> IntVal (x+y)
                             | (IntVal(x), Minus, IntVal(y)) -> IntVal (x-y)
                             | (IntVal(x), Times, IntVal(y)) -> IntVal (x*y)
                             | (IntVal(x), Eq, IntVal(y)) -> if x=y then BoolVal(true)
                                                             else BoolVal(false)
                             | (IntVal(x), Gt, IntVal(y)) -> if x>y then BoolVal(true)
                                                             else BoolVal(false)
                             | (x, Cons, y) -> (match y with
                                                ListVal(NilVal) -> ListVal(ConsVal(x, NilVal))      (* Form of x::[] *)
                                              | ListVal(ConsVal(head, tail)) -> ListVal(ConsVal(x, ConsVal(head, tail))) 
                                              (* If the right hand side has elements, then break it up into head and tail, 
                                              so we can continually add it to the list without creating nested lists *)
                                              | _ -> raise DynamicTypeError 
                                                )                   
                             | _ -> raise DynamicTypeError
                           )
      | If(e1, e2, e3) -> (match (evalExpr e1 env) with
                            BoolVal(true) -> (evalExpr e2 env)
                            | BoolVal(false) -> (evalExpr e3 env)
                            | _ -> raise DynamicTypeError
                        )
      | Function(p, e) -> FunctionVal(None, p, e, env)  
      (* p holds the parameter, e holds the expression, env holds current environment *)
      (* None option since it is not recursive and it doesn't carry its own name *)

      (* e1 is function, e2 is parameter *)
      | FunctionCall(e1, e2) -> (let f = (evalExpr e1 env) in
                                  match f with
                                 FunctionVal(Some name, fp1, fe1, fenv1) -> let value = (evalExpr e2 env) in
                                                                            let valEnv = (patMatch fp1 value) in
                                                                            let newEnv = Env.combine_envs fenv1 valEnv in
                                                                            let allEnv = Env.add_binding name f newEnv in
                                                                            (evalExpr fe1 allEnv)
                                                                            (*
                                                                              Evaluate the 2nd expression and create a new environment for it
                                                                              because it is the environment that arises from pattern matching.
                                                                              Combine this environment with the environment that produced 
                                                                              FunctionVal(Some....) for static scoping.
                                                                              Lastly, add the function value associated with the name into the 
                                                                              environment, so it handles recursion correctly
                                                                              Evaluate the expression returned by FunctionVal in this new environment
                                                                            *)

                                | FunctionVal(None, fp2, fe2, fenv2) -> let value = (evalExpr e2 env) in
                                                                        let valEnv = (patMatch fp2 value) in
                                                                        let newEnv = Env.combine_envs fenv2 valEnv in
                                                                        (evalExpr fe2 newEnv)
                                                                        (* Evaluate the 2nd expression and create a new environment for it, 
                                                                        in other words this is the argument the function was called with
                                                                        Combine this new environment with the environment that's in the 
                                                                        scope of FunctionVal
                                                                        Evaluate the expression returned by FunctionVal in the updated environment, 
                                                                        so it uses the argument FunctionCall was called with *)
                                | _ -> raise DynamicTypeError
                              ) 

      | Match(expr, l) -> match l with 
                      [] -> raise MatchFailure   (* There are no more patterns to match *) 
                      | (p1, e1)::rest -> let result = evalExpr expr env in  (* Store what expr should evaluate to in result *)
                                          try (let newEnv = patMatch p1 result in               
                                                evalExpr e1 (Env.combine_envs env newEnv))
                                          (* Attempt to match the result with the given pattern and store it in newEnv
                                            if it succeeds, then evaluate the corresponding expression for the matched pattern 
                                            in the updated environment*)
                                        with 
                                            MatchFailure -> evalExpr (Match(expr, rest)) env
                                          (* If there wasn't a match, continue matching on the rest of the tuples that 
                                            are in the form of (mopat, moexpr) *)
      | _ -> raise DynamicTypeError


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's top-level expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
      | Let(x, e) -> (Some x, evalExpr e env)   
      (* Needs to call evalExpr because you can bind anything to a variable *)
      | LetRec(f, p, e) -> (Some f, FunctionVal(Some f, p, e, env)) 
      (* Doesn't need to call evalExpr because a recursive function can only return a function *)
      | _ -> raise DynamicTypeError

