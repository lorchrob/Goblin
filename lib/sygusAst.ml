type sygus_ast = 
| Node of string * sygus_ast list 
| BVLeaf of int * bool list 
| IntLeaf of int
| BLLeaf of bool list
| VarLeaf of string (* DANIYAL: Placeholder can go here *)

let pp_print_sygus_ast: Format.formatter -> sygus_ast -> unit 
= fun ppf sygus_ast -> 
  let rec pp_print_sygus_ast' ppf sygus_ast = match sygus_ast with 
  | Node (constructor, subterms) -> 
    Format.fprintf ppf "(%s %a)"
    constructor 
    (Lib.pp_print_list pp_print_sygus_ast' " ") subterms 
  | BVLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "#b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  | VarLeaf id -> Format.pp_print_string ppf id;
  | IntLeaf d -> Format.pp_print_int ppf d;
  (* This is kind of cheating, but I don't feel like matching cvc5 format exactly *)
  | BLLeaf bits -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "#bl%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  in 
  Format.fprintf ppf "%a\n" 
  pp_print_sygus_ast' sygus_ast

let serialize: Format.formatter -> sygus_ast -> unit 
= fun ppf sygus_ast -> 
  let rec pp_print_sygus_ast' ppf sygus_ast = match sygus_ast with 
  | Node (_, subterms) -> 
    Format.fprintf ppf "%a"
    (Lib.pp_print_list pp_print_sygus_ast' "") subterms 
  | BLLeaf bits
  | BVLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  | VarLeaf _ -> failwith "Internal error: serializing final packet, but encountered leaf variable (possibly uncomputed dependent term)"
  | IntLeaf i -> Format.pp_print_int ppf i
  in 
  Format.fprintf ppf "%a\n" 
  pp_print_sygus_ast' sygus_ast