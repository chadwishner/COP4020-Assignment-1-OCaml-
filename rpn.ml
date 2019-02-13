(*Chad Wishner 66315393*)

let rpn_evl s =
  let calculate op = function
    | a::b::c -> (op b a)::c
    | _ -> invalid_arg "Not a valid expression" in
      let operators o = function 
        | "*" -> calculate( *. ) o
        | "/" -> calculate( /. ) o
        | "+" -> calculate( +. ) o
        | "-" -> calculate( -. ) o
        | "^" -> calculate( ** ) o
        | str -> (float_of_string str) :: o in
        let list = Str.split(Str.regexp " +") s in  
          List.fold_left operators [] list

let input = read_line();;
(*let quit = ref false in*)
(*while true do()
  let input = read_line();;
  print_string("Result: ");;
  print_float(List.hd(rpn_evl input));;
  print_newline();;
done;;*)

let rec exe =
    (*let input = read_line();;*)
    print_string("Result: ");;
    print_float(List.hd(rpn_evl input));;
    print_newline();;
    exe;;

exe;;

(*let math op = function
  | a::b::c -> (op b a)::c
  | _ -> invalid_arg "Not a valid expression" *)

(*let operators o = function 
  (*if op = "*" then math( *. ) o
  else if op = "/" then math( /. ) o
  else if op = "+" then math( +. ) o
  else if op = "-" then math( -. ) o
  else if op = "^" then math( ** ) o
  else if str then (float_of_string str) :: o
  (*else ->invalid_arg "User inputted an unrecognized character"*)*)

  | "*" -> math( *. ) o
  | "/" -> math( /. ) o
  | "+" -> math( +. ) o
  | "-" -> math( -. ) o
  | "^" -> math( ** ) o
  | str -> (float_of_string str) :: o *)