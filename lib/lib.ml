let rec pp_print_list pp sep ppf = function 
  | [] -> ()
  | e :: [] -> 
    pp ppf e
  | e :: tl -> 
    pp_print_list pp sep ppf [e]; 
    Format.fprintf ppf sep; 
    pp_print_list pp sep ppf tl

let print_newline: Format.formatter -> unit 
= fun ppf -> 
  Format.fprintf ppf "\n"

let all_equal lst =
  match lst with
  | [] -> true
  | x :: xs -> List.for_all ((=) x) xs

let is_even n = 
  n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent