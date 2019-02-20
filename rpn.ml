type token = 
  | Num of float
  | Op of string
  | Terror of string

let toToken (str: string) : token = 
      match str with
      | "+" | "-" | "*" | "/" -> Op(str)
      | _ -> let f=float_of_string_opt(str) in 
          match f with
              | Some(flt) -> Num(flt)
              | None -> Terror(str)

type state =
  | MyStack of float list
  | Serror of string 

let evalOp (s: string) (op1: float) (op2: float) : float =
	match s with
    | "+" -> op1 +. op2
    | "-" -> op1 -. op2
    | "*" -> op1 *. op2
    | "/" -> op1 /. op2
    | "^" -> op1 ** op2
		| _ -> 0.0

let nextState (st: state) (t: token) : state =
  match st with
    | Serror(str) -> st
    | MyStack(lst) -> 
      match t with
        | Num(f) -> MyStack(f :: lst)
        | Op(s) -> (
          match lst with
            | op2::op1::tail -> MyStack( evalOp s op1 op2 :: tail)
            | _ -> Serror("Not enough arguments for " ^ s)
        )
        | Terror(s) -> Serror("Unknown token " ^ s)

let finalState state =
  match state with
  | MyStack(stack) -> (
    match stack with
      | op::[] -> (print_float(op);
        print_newline();
      )
      | _ -> print_string("Not enough operators"); 
    )
  | Serror(err) -> print_string(err);;

let procRPN str =
  str |> 
    String.split_on_char ' ' |> 
      List.map toToken |>
        List.fold_left nextState (MyStack []) |>
          finalState;;

  
let rec exe () =  
  match read_line () with
    | "stop" -> print_endline "Ending Program" (* End Program *)
    | _ as input -> let () = procRPN input in exe();; (* Take input in, Call function for printing, recursively call exe *)   
    
(* Start Program*)
exe ();;