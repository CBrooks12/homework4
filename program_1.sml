(* Homework 4 *)

(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

use "parser.sml";

(* Here is a result datatype *)
datatype result =
    RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env); (*  We added this *)
    and env = Env of (string * result) list
(* Here is a basic environment implementation *)
exception not_found;
datatype env = Env of (string * result) list

fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun extend_env_all (Env(oldenv), id_value_list) = Env(id_value_list @ oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

(* WE WRITE THIS FUNCTION STATICALLY SCOPED: (term * environment) -> result 
PROBLEM 5 *)
fun interp_static (exp, env) = 
  
  case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x 
  | AST_BOOL b                  => RES_BOOL b 
  | AST_SUCC                    => RES_SUCC 
  | AST_PRED                    => RES_PRED 
  | AST_ISZERO                  => RES_ISZERO 
  | AST_IF (exp1, exp2, exp3)   => let val v1 = interp (exp1, env)  
                                    in 
                                    if v1 = RES_BOOL(true)
                                        then interp_static (exp2, env)
                                    else if v1 = RES_BOOL(false)
                                        then interp_static (exp3, env)
                                    else RES_ERROR "boolean error"
                                    end
  | AST_APP (exp1, exp2)        =>  case (interp_static(exp1, env), interp_static(exp2,env)) 
                                    of
                                    (AST_ERROR s, AST_ERROR s)         => RES_ERROR s
                                    | (AST_ERROR s,Env(nil))           => RES_ERROR s
                                    | (Env(nil), AST_ERROR)            => RES_ERROR s
                                    | (AST_SUCC, Env(nil))             => RES_ERROR "forgot to add number to SUCC"  
                                    | (AST_SUCC, AST_NUM x)            => RES_NUM (x+1)
                                    | (AST_PRED, Env(nil))             => RES_ERROR "forgot to add number to PRED"
                                    | (AST_PRED, AST_NUM x)            => RES_NUM (x-1)
                                    | (AST_ISZERO, Env(nil))           => RES_ERROR "forgot number to compare to iszero"
                                    | (AST_ISZERO, AST_NUM x)          => if x=0 
                                                                          then RES_BOOL(true)
                                                                          else RES_BOOL(false)
                                    | (AST_FUN(var, exp), AST_NUM x)   => RES_FUN var exp
   | AST_ID name                 => lookup_env(env, name)
   | AST_FUN (var, exp)          => RES_CLOSURE(var, exp, env) 



(*  Once you have defined interp, you can try out simple examples by
      interp (parsestr "succ (succ 7)"), new_env());
    and you can try out larger examples by
      interp (parsefile "your-file-here", new_env());
*)