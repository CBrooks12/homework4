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
  | RES_ID 	  of string
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env) (*  We added this *)
    and env = Env of (string * result) list;
(* Here is a basic environment implementation *)
exception not_found;

fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun extend_env_all (Env(oldenv), id_value_list) = Env(id_value_list @ oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

(* Problem 5 *)		
fun interp_static(exp, env) = 

  case exp of
	AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x 
  | AST_BOOL b                  => RES_BOOL b 
  | AST_SUCC                    => RES_SUCC 
  | AST_PRED                    => RES_PRED 
  | AST_ISZERO                  => RES_ISZERO 
  | AST_IF (exp1, exp2, exp3)   =>  let val v1 = interp_static(exp1, env)  
									in 
										if v1 = RES_BOOL(true)
											then interp_static(exp2, env)
										else if v1 = RES_BOOL(false)
											then interp_static(exp3, env)
										else RES_ERROR "boolean error"
									end
	| AST_ID name                 => lookup_env(env, name)
	| AST_FUN (var, exp)          => RES_CLOSURE(var, RES_FUN(var, exp), env)

	| AST_APP (exp1, exp2)        =>  case (interp_static(exp1,env), interp_static(exp2,env)) of
										(RES_ERROR s, _) 			=> RES_ERROR s
										| (RES_SUCC, RES_NUM n) 	=> RES_NUM(n+1)  
										| (RES_PRED, RES_NUM n) 	=> if n = 0
																		then RES_NUM 0 
																		else RES_NUM(n-1)
										| (RES_ISZERO, RES_NUM n)  	=> if n = 0
																		then RES_BOOL(true)
																		else RES_BOOL(false)
										| (RES_FUN(x, e), e2) 		=> let  (* e2 is the interpreted version of exp2 *)
																			val newEnv = extend_env(env, x, e2) 
																		in
																			interp_static(exp1, newEnv)
																		end
										| (RES_CLOSURE(var, RES_FUN(x, e), enf), e2) => 
																		let
																			val en = extend_env(enf, x, e2)
																		in
																			interp_static(exp1, en)
																		end
										| (RES_ID s, e2)			=> let
																			val newEnv2 = extend_env(env, s, e2)
																		in
																			interp_static(exp1, newEnv2)
																		end
										| (_, _)					=> RES_ERROR "not a valid functional application"

(*  Once you have defined interp, you can try out simple examples by
      interp ((parsestr "succ (succ 7)"), new_env());
    and you can try out larger examples by
      interp (parsefile "your-file-here", new_env());
*)