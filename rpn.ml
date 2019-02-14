(*Chad Wishner 66315393*)

let rpn_evl s =
  let calculate op = function
    | a::b::c -> (op b a)::c (* Use the OCaml native operations *)
    | _ -> invalid_arg "Not a valid expression" in
      (* Match the operations and call calculate function *)
      let operators o = function 
        | "*" -> calculate( *. ) o
        | "/" -> calculate( /. ) o
        | "+" -> calculate( +. ) o
        | "-" -> calculate( -. ) o
        | "^" -> calculate( ** ) o
        | str -> (float_of_string str) :: o in
        let list = Str.split(Str.regexp " +") s in (* Turn the input string to a list of strings *) 
          List.fold_left operators [] list (* Operate on list pre-application and store in accumulator *)

(* Printing the result and calling the evaluation function *)
let do_stuff input = 
  print_string("Result: "); 
  print_float(List.hd(rpn_evl input)); (* Print the first element in the list after doing evaluations on the string input *)
  print_newline();;

(* Recursive function to take recurring input *)
let rec exe () =  
  match read_line () with
    | "stop" -> print_endline "Ending Program" (* End Program *)
    | _ as input -> let () = do_stuff input in exe();; (* Take input in, Call function for printing, recursively call exe *)   

(* Start Program*)
exe ();;