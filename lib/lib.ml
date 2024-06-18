let rec pp_print_list pp sep ppf = function 
  | [] -> ()
  | e :: [] -> 
    pp ppf e
  | e :: tl -> 
    pp_print_list pp sep ppf [e]; 
    Format.fprintf ppf sep; 
    pp_print_list pp sep ppf tl

let all_equal lst =
  match lst with
  | [] -> true
  | x :: xs -> List.for_all ((=) x) xs