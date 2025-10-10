(* TODO: Distinguish between strings, placeholders, dep terms in solver_ast type *)
(* Using solver asts to represent two different things. 
      1. Lisp-style terms compatible with any input grammar, and 
      2. SMT solver models upon calling check-sat and get-model 
         (essentially a single node with a list of leaf children) *)

type concrete_set = 
| StringSet of Utils.StringSet.t 

type solver_ast = 
| Node of (string * int option) * solver_ast list 
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

let pp_print_solver_ast: Format.formatter -> solver_ast -> unit 
= fun ppf solver_ast -> 
  let rec pp_print_solver_ast' ppf solver_ast = match solver_ast with 
  | Node ((constructor, Some idx), subterms) -> 
    Format.fprintf ppf "(%s%d %a)"
    constructor idx
    (Lib.pp_print_list pp_print_solver_ast' " ") subterms 
  | Node ((constructor, None), subterms) -> 
    Format.fprintf ppf "(%s %a)"
    constructor 
    (Lib.pp_print_list pp_print_solver_ast' " ") subterms 
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
  pp_print_solver_ast' solver_ast
