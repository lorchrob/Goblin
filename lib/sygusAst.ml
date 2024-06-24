type sygus_ast = 
| Node of string * sygus_ast list 
| BVLeaf of int * bool list 
| IntLeaf of int
| BLLeaf of bool list

let pp_print_sygus_ast: Format.formatter -> sygus_ast -> unit 
= fun ppf sygus_ast -> 
  let rec pp_print_sygus_ast' ppf sygus_ast = match sygus_ast with 
  | Node (constructor, subterms) -> 
    Format.fprintf ppf "(%s %a)"
    constructor 
    (Lib.pp_print_list pp_print_sygus_ast' " ") subterms 
  | BVLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  | IntLeaf _ 
  | BLLeaf _ -> assert false
  in 
  Format.fprintf ppf "%a\n" 
  pp_print_sygus_ast' sygus_ast

let serialize: Format.formatter -> sygus_ast -> unit 
= fun ppf sygus_ast -> 
  let rec pp_print_sygus_ast' ppf sygus_ast = match sygus_ast with 
  | Node (_, subterms) -> 
    Format.fprintf ppf "%a"
    (Lib.pp_print_list pp_print_sygus_ast' "") subterms 
  | BVLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  | IntLeaf _ 
  | BLLeaf _ -> assert false
  in 
  Format.fprintf ppf "%a\n" 
  pp_print_sygus_ast' sygus_ast