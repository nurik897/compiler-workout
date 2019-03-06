(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    (* Cast bool to int *)
let to_int b = if b then 1 else 0

(* Cast int to bool *)
let to_bool i = i != 0

(* Cast (int->int->bool) to (int->int->int) *)
let c_op1 f = fun a b -> to_int (f a b)

(* Cast (bool->bool->bool) to (int->int->int) *)
let c_op2 f = fun a b -> to_int (f (to_bool a) (to_bool b))

(* Source operator into OCaml operator *)
let eval_op op = match op with
    | "!!" -> c_op2 ( || )
    | "&&" -> c_op2 ( && )
    | "==" -> c_op1 ( = )
    | "!=" -> c_op1 ( <> )
    | "<=" -> c_op1 ( <= )
    | "<"  -> c_op1 ( < )
    | ">=" -> c_op1 ( >= )
    | ">"  -> c_op1 ( > )
    | "+"  -> ( + )
    | "-"  -> ( - )
    | "*"  -> ( * )
    | "/"  -> ( / )
    | "%"  -> ( mod )
    | _    -> failwith (Printf.sprintf "Unknown operator");;


let rec eval s e = match e with
    | Const n          -> n
    | Var x            -> s x
    | Binop (op, l, r) -> eval_op op (eval s l) (eval s r);;

let parseBinOp op = ostap(- $(op)), (fun x y -> Binop (op, x, y))

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      expr:
            !(Ostap.Util.expr
                (fun x -> x)
                (Array.map (fun (assoc, ops) -> assoc, List.map parseBinOp ops)
                  [|
                    `Lefta, ["!!"];
                    `Lefta, ["&&"];
                    `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
                    `Lefta, ["+"; "-"];
                    `Lefta, ["*"; "/"; "%"];
                  |]
               )
               primary
            );

        primary: n:DECIMAL {Const n} | x:IDENT {Var x} | -"(" expr -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config statement = 
    let (state, input, output) = config in
    match statement with
      | Read variable_name -> (match input with
        | head::tail -> (Expr.update variable_name head state, tail, output))
      | Write expression -> (state, input, output @ [Expr.eval state expression])
      | Assign (variable_name, expression) -> (Expr.update variable_name (Expr.eval state expression) state, input, output)
      | Seq (state1, state2) -> eval (eval config state1) state2;;

    (* Statement parser *)
    ostap (
      line: "read"  "("  x:IDENT        ")" {Read x}
          | "write" "("  e:!(Expr.expr) ")" {Write e}
          | x:IDENT ":=" e:!(Expr.expr)     {Assign (x, e)};

        parse: l:line ";" rest:parse {Seq (l, rest)} | line
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
