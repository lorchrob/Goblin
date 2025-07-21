(* TODO: Distinguish between strings, placeholders, dep terms in sygus_ast type *)
type sygus_ast = 
| Node of string * sygus_ast list 
| BVLeaf of int * bool list 
| IntLeaf of int
| BLLeaf of bool list
| BoolLeaf of bool
| StrLeaf of string
| VarLeaf of string (* Placeholder can go here *)

type endianness = 
| Little 
| Big

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
  | VarLeaf id -> Format.fprintf ppf "%s" id;
  | StrLeaf id -> Format.fprintf ppf "%S" id;
  | IntLeaf d -> Format.pp_print_int ppf d;
  | BoolLeaf b -> Format.pp_print_bool ppf b;
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
  | VarLeaf id 
  | StrLeaf id -> Format.fprintf ppf "%s" id;
  | IntLeaf i -> Format.pp_print_int ppf i
  | BoolLeaf b -> Format.pp_print_bool ppf b
  in 
  Format.fprintf ppf "%a\n" 
  pp_print_sygus_ast' sygus_ast

let bools_to_bytes endianness bools =
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
  let byte_list = 
    if endianness = Big then bools_to_byte_list bools |> List.rev 
    else bools_to_byte_list bools 
  in
  Bytes.of_string (String.concat "" (List.map (String.make 1) (List.map char_of_int byte_list)))

(*******************************************
    SERIALIZATION OF SYGUS_AST TO BYTES
********************************************)

type metadata = {
  var_leaf_count : int;
  var_leaf_info : (int * int) list; (* (Offset, Length) *)
}

let int_to_byte n =
  if n < 0 || n > 255 then
    Utils.crash "Integer out of range for 1 byte representation";
  let bytes = Bytes.create 1 in
  Bytes.set bytes 0 (Char.chr n);
  bytes

let encode_metadata metadata =
  let num_var_leaves = metadata.var_leaf_count in
  let metadata_bytes = ref (Bytes.of_string "") in
  metadata_bytes := int_to_byte num_var_leaves;
  List.iter (fun (offset, len) ->
    metadata_bytes := Bytes.cat (int_to_byte len) !metadata_bytes ;
    metadata_bytes := Bytes.cat (int_to_byte offset) !metadata_bytes 
  ) metadata.var_leaf_info;
  !metadata_bytes

(* Second component of output is metadata about placeholders.
   The last byte of metadata encodes the number of placeholders.
   Each placeholder has an associated offset and length (both in BYTES, not BITS).

   Metadata structure with 3 placeholders (ph = placeholder):

   Byte1       Byte2       Byte3       Byte4       Byte5       Byte6       Byte7 
   ph1_offset  ph1_len     ph2_offset  ph2_len     ph3_offset  ph3_len     0x00 0x03
                                                                           (there are 3 placeholders)
*)
let serialize_bytes: endianness -> sygus_ast -> bytes * bytes 
= fun endianness sygus_ast -> 
  let rec serialize_aux endianness sygus_ast offset acc_metadata =
    match sygus_ast with
    | Node (id, subterms) ->
      let regex = Str.regexp "rg_id_list_con[0-9]+" in
      let is_match = Str.string_match regex id 0 in
      let endianness = if is_match then Little else Big in
      List.fold_left
      (fun (acc_bytes, acc_metadata, current_offset) term ->
        let term_bytes, term_metadata, new_offset = serialize_aux endianness term current_offset acc_metadata in
        (Bytes.cat acc_bytes term_bytes, term_metadata, new_offset))
      (Bytes.empty, acc_metadata, offset) subterms
      
    | BLLeaf bits
    | BVLeaf (_, bits) ->
      let bit_bytes = bools_to_bytes endianness bits in
      (bit_bytes, acc_metadata, offset + Bytes.length bit_bytes)
      
    | VarLeaf id ->
      let var_leaf_data = Bytes.of_string id in
      let var_leaf_length = Bytes.length var_leaf_data in
      let new_metadata = {
        var_leaf_count = acc_metadata.var_leaf_count + 1;
        var_leaf_info = (offset, var_leaf_length) :: acc_metadata.var_leaf_info
      } in
      (var_leaf_data, new_metadata, offset + var_leaf_length)
      
    | BoolLeaf _ -> Utils.crash "serializing final packet, unhandled case 1"
    | IntLeaf _ -> Utils.crash "serializing final packet, unhandled case 2"
    | StrLeaf _ -> Utils.crash "serializing final packet, unhandled case 3"
  in
  let initial_metadata = {
    var_leaf_count = 0;
    var_leaf_info = []
  } in
  let serialized_bytes, final_metadata, _ = serialize_aux endianness sygus_ast 0 initial_metadata in
  let metadata_bytes = encode_metadata final_metadata in
  serialized_bytes, metadata_bytes

(* For a concrete example, try the following in main.ml: 

  let ast = Parsing.parse
  "
  <S> ::= <A> <Placeholder> <B> <Placeholder2> <C> { <Placeholder> <- \"testa\"; <Placeholder2> <- \"testb\";};
  <A> :: BitVector(16) { <A> <- int_to_bitvector(16, 256); }; 
  <B> :: BitVector(8) { <B> <- int_to_bitvector(8, 256); }; 
  <C> :: BitVector(8) { <C> <- int_to_bitvector(8, 256); }; 
  <Placeholder> :: String;
  <Placeholder2> :: String;
  " in 
  match Pipeline.sygusGrammarToPacket ast with
  | Ok (bytes, metadata) -> 
    Utils.print_bytes_as_hex bytes ;
    Utils.print_bytes_as_hex metadata
  | Error _ -> print_endline "error"
  
  *)
