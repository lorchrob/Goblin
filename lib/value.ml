(* Concrete values of symbolic terminals (leaves of generated terms and SMT model values) *)

type t =
| Bool of bool
| Int of int
| String of string
| Placeholder of string
| BitVector of int * bool list (* width, bits (most significant first) *)
| BitList of bool list
| StringSet of Utils.StringSet.t
| Unit

let ty: t -> Ast.il_type = function
| Bool _ -> Bool
| Int _ -> Int
| String _ -> String
| Placeholder _ -> Placeholder
| BitVector (width, _) -> BitVector width
| BitList _ -> BitList
| StringSet _ -> Set String
| Unit -> Unit

let pp_print_bits ppf bits =
  Lib.pp_print_list Format.pp_print_int "" ppf (List.map Bool.to_int bits)

let pp_print_smt_int ppf i =
  if i >= 0 then Format.pp_print_int ppf i
  else Format.fprintf ppf "(- %d)" (-1 * i)

let pp_print_smt_bool_seq ppf bits = match bits with
| [] -> Format.fprintf ppf "seq.empty"
| [b] -> Format.fprintf ppf "(seq.unit %b)" b
| _ :: _ :: _ ->
  Format.fprintf ppf "(seq.++ ";
  List.iter (fun b -> Format.fprintf ppf "(seq.unit %b) " b) bits;
  Format.fprintf ppf ")"

let rec smtlib_of_stringset set =
  match Utils.StringSet.elements set with
  | [] -> "(as set.empty (Set String))"
  | [x] -> Printf.sprintf "(set.singleton \"%s\")" x
  | x :: xs ->
    Printf.sprintf "(set.union (set.singleton \"%s\") %s)" x
      (smtlib_of_stringset (Utils.StringSet.of_list xs))

(* SMT-LIB syntax *)
let pp_smt ppf = function
| Bool b -> Format.pp_print_bool ppf b
| Int i -> pp_print_smt_int ppf i
| Placeholder str -> Format.pp_print_string ppf str
| String str -> Format.fprintf ppf "\"%s\"" str
| StringSet set -> Format.pp_print_string ppf (smtlib_of_stringset set)
| BitVector (_, bits) -> Format.fprintf ppf "#b%a" pp_print_bits bits
| BitList bits -> pp_print_smt_bool_seq ppf bits
| Unit -> Format.fprintf ppf "()"

(* Goblin's S-expression output syntax *)
let pp ppf = function
| Bool b -> Format.pp_print_bool ppf b
| Int i -> pp_print_smt_int ppf i
| String str | Placeholder str -> Format.fprintf ppf "\"%s\"" str
| StringSet set -> Format.pp_print_string ppf (smtlib_of_stringset set)
| BitVector (_, bits) -> Format.fprintf ppf "0b%a" pp_print_bits bits
| BitList bits -> pp_print_smt_bool_seq ppf bits
| Unit -> Format.fprintf ppf "()"
