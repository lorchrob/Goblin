(* TODO: Distinguish between strings, placeholders, dep terms in sygus_ast type *)
(* Using sygus asts to represent two different things. 
      1. Lisp-style terms compatible with any input grammar, and 
      2. SMT solver models upon calling check-sat and get-model 
         (essentially a single node with a list of leaf children) *)

type concrete_set = 
| StringSet of Utils.StringSet.t 

type sygus_ast = 
| Node of (string * int option) * sygus_ast list 
| BVLeaf of int * bool list 
| IntLeaf of int
| BLLeaf of bool list
| BoolLeaf of bool
| StrLeaf of string
| SetLeaf of concrete_set 
| VarLeaf of string 
| UnitLeaf

(*!!
type expr = 
| Op of expr * expr 
| ...

type q = Forall | Exists

(* First string is variable name, second string is type *)
type bound_vars = (string * string) list

type formula = 
| Quantifier of q * bound_vars * formula
| Implies of formula * formula
| Equality of expr * expr 
| Disequality of expr * expr

type ast = formula list
*)
type endianness = 
| Little 
| Big

let rec smtlib_of_stringset set =
  match Utils.StringSet.elements set with
  | [] ->
      "(as set.empty (Set String))"
  | [x] ->
      Printf.sprintf "(set.singleton \"%s\")" x
  | x :: xs ->
      let rest = Utils.StringSet.of_list xs in
      Printf.sprintf "(set.union %s %s)"
        (Printf.sprintf "(set.singleton \"%s\")" x)
        (smtlib_of_stringset rest)

let pp_print_sygus_ast: Format.formatter -> sygus_ast -> unit 
= fun ppf sygus_ast -> 
  let rec pp_print_sygus_ast' ppf sygus_ast = match sygus_ast with 
  | Node ((constructor, Some idx), subterms) -> 
    Format.fprintf ppf "(%s%d %a)"
    constructor idx
    (Lib.pp_print_list pp_print_sygus_ast' " ") subterms 
  | Node ((constructor, None), subterms) -> 
    Format.fprintf ppf "(%s %a)"
    constructor 
    (Lib.pp_print_list pp_print_sygus_ast' " ") subterms 
  | BVLeaf (_, bits) -> 
    let bits = List.map Bool.to_int bits in
    Format.fprintf ppf "0b%a"
    (Lib.pp_print_list Format.pp_print_int "") bits
  | VarLeaf id -> Format.fprintf ppf "\"%s\"" id;
  | StrLeaf id when String.equal id "infeasible" -> Format.fprintf ppf "infeasible" 
  | StrLeaf id -> Format.fprintf ppf "\"%s\"" id;
  | IntLeaf d -> 
    if d >= 0 then 
      Format.pp_print_int ppf d
    else 
      Format.fprintf ppf "(- %d)" (d * -1)
  | BoolLeaf b -> Format.pp_print_bool ppf b;
  | UnitLeaf -> Format.fprintf ppf "()" 
  | SetLeaf (StringSet s) -> 
    Format.pp_print_string Format.std_formatter (smtlib_of_stringset s)
  | BLLeaf bits ->

let print_smt_bool_seq fmt (lst : bool list) : unit =
  match lst with
  | [] ->
    Format.fprintf fmt "seq.empty"
  | [b] ->
    if b then Format.fprintf fmt "(seq.unit true)"
    else Format.fprintf fmt "(seq.unit false)"
  | _ ->
    Format.fprintf fmt "(seq.++ ";
    let print_unit b =
      if b then Format.fprintf fmt "(seq.unit true) "
      else Format.fprintf fmt "(seq.unit false) "
    in
    List.iter print_unit lst;
    Format.fprintf fmt ")"
  in print_smt_bool_seq ppf bits
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
  | UnitLeaf -> ()
  | BoolLeaf b -> Format.pp_print_bool ppf b
  | SetLeaf (StringSet s) -> 
    Format.fprintf Format.std_formatter "{%a}" 
      (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringSet.to_list s)
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

let flip_endianness e = match e with 
| Little -> Big 
| Big -> Little 

(* Second component of output is metadata about placeholders.
   The last byte of metadata encodes the number of placeholders.
   Each placeholder has an associated offset and length (both in BYTES, not BITS).

   Metadata structure with 3 placeholders (ph = placeholder):

   Byte1       Byte2       Byte3       Byte4       Byte5       Byte6       Byte7 
   ph1_offset  ph1_len     ph2_offset  ph2_len     ph3_offset  ph3_len     0x00 0x03
                                                                           (there are 3 placeholders)

  `exception_list` denotes the list of fields to flip the default endianness (eg if default endianness is Big,
  `serialize_bytes` will encode fields from `exception_list` as little endian.
*)
let serialize_bytes: endianness -> string list -> sygus_ast -> bytes * bytes 
= fun default_endianness exception_list sygus_ast -> 
  let rec serialize_aux endianness exception_list sygus_ast offset acc_metadata =
    match sygus_ast with
  | Node ((id, _), subterms) ->
      if id.[0] = '_' then Bytes.empty, acc_metadata, offset else
      let is_match = List.exists (fun str -> 
        let regex = Str.regexp (String.lowercase_ascii str ^ "_con[0-9]+") in 
        Str.string_match regex (String.lowercase_ascii id) 0 || 
        Utils.str_eq_ci id str
      ) exception_list in
      let endianness = if is_match then flip_endianness default_endianness else default_endianness in
      List.fold_left
      (fun (acc_bytes, acc_metadata, current_offset) term ->
        let term_bytes, term_metadata, new_offset = serialize_aux endianness exception_list term current_offset acc_metadata in
        (Bytes.cat acc_bytes term_bytes, term_metadata, new_offset))
      (Bytes.empty, acc_metadata, offset) subterms
      
    | BLLeaf bits
    | BVLeaf (_, bits) ->
      let bit_bytes = bools_to_bytes endianness bits in
      (bit_bytes, acc_metadata, offset + Bytes.length bit_bytes)
      
    | StrLeaf id 
    | VarLeaf id ->
      let var_leaf_data = Bytes.of_string id in
      let var_leaf_length = Bytes.length var_leaf_data in
      let new_metadata = {
        var_leaf_count = acc_metadata.var_leaf_count + 1;
        var_leaf_info = (offset, var_leaf_length) :: acc_metadata.var_leaf_info
      } in
      (var_leaf_data, new_metadata, offset + var_leaf_length)
      
    | BoolLeaf b ->  
      let bits = [b] in 
      let bit_bytes = bools_to_bytes endianness bits in
      (bit_bytes, acc_metadata, offset + Bytes.length bit_bytes)

    | IntLeaf _ -> Utils.crash "serializing final packet, unhandled case 2"
    | SetLeaf _ -> Utils.crash "serializing final packet, unhandled case 3" 
    | UnitLeaf  -> Utils.crash "serializing final packet, unhandled case 4"
  in
  let initial_metadata = {
    var_leaf_count = 0;
    var_leaf_info = []
  } in
  let serialized_bytes, final_metadata, _ = serialize_aux default_endianness exception_list sygus_ast 0 initial_metadata in
  let metadata_bytes = encode_metadata final_metadata in
  serialized_bytes, metadata_bytes

let bytes_to_bools_be (bs : bytes) : bool list =
  let n = Bytes.length bs in
  let rec outer i acc =
    if i = n then List.rev acc
    else
      let v = Char.code (Bytes.get bs i) in
      let rec inner bit acc' =
        if bit < 0 then acc'
        else
          let b = ((v lsr bit) land 1) = 1 in
          inner (bit - 1) (b :: acc')
      in
      outer (i + 1) (inner 7 acc)
  in
  outer 0 []

let pad_bits (bits : bool list) : bool list =
  let len = List.length bits in
  (* faster mod 8 *)
  let rem = len land 7 in                    
  if rem = 0 then bits
  else
    let k = 8 - rem in
    let rec mk n acc = if n = 0 then acc else mk (n - 1) (false :: acc) in
    bits @ mk k []

let signed_min_width_bytes_int64 (n:int64) : int =
  let open Int64 in
  let fits w =
    let bits = 8 * w in
    let minv = neg (shift_left 1L (bits - 1)) in
    let maxv = sub (shift_left 1L (bits - 1)) 1L in
    n >= minv && n <= maxv
  in
  let rec find w =
    if w > 8 then invalid_arg "signed value needs >8 bytes; use bigints"
    else if fits w then w else find (w + 1)
  in
  find 1

let int64_to_bytes_min
    endianness
    (n:int64)
  : bytes =
  let open Int64 in
  let width = signed_min_width_bytes_int64 n in
  let b = Bytes.create width in
  begin match endianness with
  | Big ->
      for i = 0 to width - 1 do
        let shift = (width - 1 - i) * 8 in
        let byte = to_int (logand (shift_right_logical n shift) 0xFFL) in
        Bytes.set b i (Char.chr byte)
      done
  | Little ->
      for i = 0 to width - 1 do
        let shift = i * 8 in
        let byte = to_int (logand (shift_right_logical n shift) 0xFFL) in
        Bytes.set b i (Char.chr byte)
      done
  end;
  b

let int_to_bytes_min endianness (n:int) : bytes =
  int64_to_bytes_min endianness (Int64.of_int n)

let serialize_bytes_packed: sygus_ast -> bytes 
= fun sygus_ast -> 
  let rec bits_of_sa sygus_ast = match sygus_ast with 
  | BLLeaf bits -> bits
  | BVLeaf (_, bits) -> bits
  | StrLeaf str | VarLeaf str -> 
    let bytes = Bytes.of_string str in 
    bytes_to_bools_be bytes 
  | IntLeaf i -> 
    let bytes = int_to_bytes_min Big i in 
    bytes_to_bools_be bytes 
  | Node (_, children) -> 
    List.concat_map bits_of_sa children
  | _ -> assert false 
  in 
  let bits = BLLeaf (bits_of_sa sygus_ast |> pad_bits) in 
  serialize_bytes Big [] bits |> fst
