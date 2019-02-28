open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

let eval_insn config insn = 
	let (stack, stmt_config) = config in
	let (state, input, output) = stmt_config in
	match insn with
	| BINOP operator -> (match stack with
		| y::x::tail -> ([(Syntax.Expr.eval_op operator) x y]@tail, stmt_config))
    | CONST value -> ([value]@stack, stmt_config)                 
	| READ -> (match input with
		| head::tail -> ([head]@stack, (state, tail, output)))
	| WRITE -> (match stack with
		| head::tail -> (tail, (state, input, output@[head])))
	| LD  variable_name -> ([state variable_name]@stack, stmt_config)
	| ST  variable_name -> (match stack with
		| head::tail -> (tail, (Syntax.Expr.update variable_name head state, input, output)))

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval config prg = List.fold_left eval_insn config prg

		
(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr expr = 
	match expr with
		| Syntax.Expr.Const const -> [CONST const]
        | Syntax.Expr.Var var -> [LD var]
        | Syntax.Expr.Binop (operator, left_expression, right_expression) -> (compile_expr left_expression)@(compile_expr right_expression)@[BINOP operator];;
		
let rec compile statement = 
	match statement with
		| Syntax.Stmt.Read variable_name -> [READ; ST variable_name]
		| Syntax.Stmt.Write expression -> (compile_expr expression)@[WRITE]
		| Syntax.Stmt.Assign (variable_name, expression) -> (compile_expr expression)@[ST variable_name]
		| Syntax.Stmt.Seq (state1, state2) -> (compile state1)@(compile state2);;
		