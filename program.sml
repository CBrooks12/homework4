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
  | RES_FUN   of (string * term);

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

(*  Here's a partial skeleton of interp : (term * environment) -> result.
    I've done the first case for you
*)
fun interp (exp, env) = 

  case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x 
  | AST_BOOL b                  => RES_BOOL b 
  | AST_SUCC                    => RES_SUCC 
  | AST_PRED                    => RES_PRED 
  | AST_ISZERO                  => RES_ISZERO 
  | AST_IF (cond, trueExp, falseExp)   (*=> if interp(exp1,env) then interp(exp2,env) else interp(exp3,env);*)
								=> let val v1 = interp (exp1, env)
									in 
									if v1 = RES_BOOL(true)
										then interp (exp2, env)
									else if v1 = RES_BOOL(false)
										then interp (exp3, env)
									else RES_ERROR "boolean error"
								   end
											
  | AST_APP (exp1, exp2)        => RES_ERROR "Not yet implemented"
  | AST_ID name                 => lookup_env(env, name)
 (* | AST_FUN  (var, exp)         => (*extend_env(env, var, (AST_NUM var))*) RES_ERROR "Not yet implemented"*)
 
 (*) | AST_APP (exp1, exp2)        => let val r1 = interp(exp1,env)
                                       val r2 = interp(exp2,env)
                                      in AST_APP(r1,r2)
                                      end;             *)                       
(*  | AST_ID name                 => lookup_env(env, name) *)
(*  | AST_FUN (var, exp)          => RES_FUN(var, exp) *)


(*  Once you have defined interp, you can try out simple examples by
      interp (parsestr "succ (succ 7)"), new_env());
    and you can try out larger examples by
      interp (parsefile "your-file-here", new_env());
*)