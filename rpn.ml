(*Chad Wishner 66315393*)

open Str;;

let math op = function
  | a::b::c -> (op b a)::c
  | _ -> invalid_arg "Not a valid expression"

let operators o = function
  | "*" -> math( *. ) o
  | "/" -> math( /. ) o
  | "+" -> math( +. ) o
  | "-" -> math( -. ) o
  | "^" -> math( ** ) o
  | str -> (float_of_string str) :: o

let delim s = Str.split(Str.regexp " +") s;;

let rpn_evl s =
  let list = delim(s) in
    List.fold_left operators [] list

let input = read_line();;
print_string("Result: ");;
print_float(rpn_evl input);;
print_newline();;
