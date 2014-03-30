(* ENVIRONMENTS *)

(* We define an abstraction to represent environments, which are mappings from
   names (strings) to items of some type.  This abstraction can be
   instantiated with a type for items, to produce things like type environments
   and value environments. *)

(* The "module type" defined below gives the interface of the
   environment module, containing the types of all the declarations in
   that module.  That should suffice for you to understand how to use
   the environment module -- you should not need to look at the actual
   implementation (but you are welcome to do so). *)

(* Environments are defined in a functional style.  For example, adding a
   new binding does not modify the original environment, but instead returns
   a new environment that is identical to the original one but also includes
   the additional binding. *)
  
(* The interface of the environment abstraction. *)
module type ENV = sig
  (* The abstract type representing environments *)
  type 'a env
    (* Thrown when lookup for some name fails in the environment *) 
  exception NotBound
  val empty_env: unit -> 'a env
    (* Add a binding to the environment, shadowing any existing binding
       with the same name. *)
  val add_binding:  string -> 'a -> 'a env -> 'a env
    (* Combine two environments, with bindings from the second one shadowing
       bindings from the first one. *)
  val combine_envs: 'a env -> 'a env -> 'a env
    (* Lookup a name in the environment, throwing NotBound if the name is not
       bound in the environment. *)
  val lookup: string -> 'a env -> 'a
end;;

(* The environment module implementation. *)
module Env : ENV = struct
  (* We represent an environment as a list of bindings, with each binding
     represented as a pair. *)
  type 'a env = (string * 'a) list;;

  exception NotBound

  (* Create an empty environment. *)
  let empty_env():'a env = []

  (* A new binding is prepended to an environment.  Any existing binding
     with the same name is shadowed, assuming lookup proceeds from front to
     back.  *)
  let add_binding (s:string) (item:'a) (e:'a env) :'a env = (s,item)::e

  (* Combine two environments, with bindings from the second one shadowing
     bindings from the first one. *)
  let combine_envs (e1:'a env) (e2: 'a env):'a env = e2@e1 

  (* Lookup a name in the environment, throwing NotBound if the name is not
     bound in the environment. *)
  let lookup (s:string) (e: 'a env) :'a =
    try List.assoc s e with
	Not_found -> raise NotBound
end;;

