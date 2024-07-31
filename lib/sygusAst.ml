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

let bools_to_bytes bools =
  (* Function to drop the first n elements of a list *)
  let rec drop n lst =
    match lst with
    | [] -> []
    | _ :: tl -> if n > 0 then drop (n - 1) tl else lst
  in
  
  (* Function to extract a sublist *)
  let sublist lst start len =
    let rec aux acc n = function
      | [] -> List.rev acc
      | hd :: tl ->
        if n <= 0 then List.rev acc
        else aux (hd :: acc) (n - 1) tl
    in
    aux [] len (drop start lst)
  in
  
  (* Convert a list of exactly 8 booleans to a byte *)
  let bools_to_byte bools =
    List.fold_left (fun acc (i, b) -> acc + (if b then 1 lsl (7 - i) else 0)) 0 (List.mapi (fun i b -> (i, b)) bools)
  in
  
  (* Main function to convert a list of booleans to a byte array *)
  let rec bools_to_byte_list bools =
    let len = List.length bools in
    if len = 0 then []
    else
      let chunk = sublist bools 0 (min 8 len) in
      let padded_chunk = chunk @ (List.init (8 - List.length chunk) (fun _ -> false)) in
      let byte = bools_to_byte padded_chunk in
      byte :: bools_to_byte_list (drop 8 bools)
    in
  
  (* Convert the list of bytes to a byte array *)
  let byte_list = bools_to_byte_list bools |> List.rev in
  Bytes.of_string (String.concat "" (List.map (String.make 1) (List.map char_of_int byte_list)))

let rec serialize_bytes: sygus_ast -> bytes 
= fun sygus_ast -> match sygus_ast with
  | Node (_, subterms) -> 
    let bytes_list = List.map serialize_bytes subterms in
    List.fold_left Bytes.cat (List.hd bytes_list) (List.tl bytes_list)
  | BLLeaf bits
  | BVLeaf (_, bits) -> bools_to_bytes bits
  | VarLeaf _ 
  | IntLeaf _ -> failwith "Internal error: serializing final packet, but encountered leaf variable (possibly uncomputed dependent term)"