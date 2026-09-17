(* Names of grammar symbols (nonterminals and symbolic terminals), 
   including the nonterminals Goblin generates for attributes and stubs *)

type t =
(* <nt> from the input grammar *)
| User of string
(* Generated nonterminal holding synthesized attribute `attr` *)
| SynthAttr of string
(* Generated nonterminal holding inherited attribute `attr` of nonterminal <nt> *)
| InhAttr of string * string
(* Generated placeholder for a part of the grammar that is solved separately
   (derived fields, divide and conquer subproblems) *)
| Stub of stub

and stub = {
  id : int; (* unique among stubs *)
  stands_for : t; (* the nonterminal that the stub replaces *)
}

let compare: t -> t -> int = compare

let equal: t -> t -> bool = (=)

let stub_counter = ref 0

let fresh_stub stands_for =
  let id = !stub_counter in
  stub_counter := id + 1;
  { id; stands_for }

let equal_stub stub1 stub2 = stub1.id = stub2.id

(* The nonterminal a stub stands for (which may itself be a stub) *)
let unstub_once nt = match nt with
| Stub { stands_for; _ } -> stands_for
| User _ | SynthAttr _ | InhAttr _ -> nt

(* The non-stub nonterminal a (possibly nested) stub stands for *)
let rec unstub nt = match nt with
| Stub { stands_for; _ } -> unstub stands_for
| User _ | SynthAttr _ | InhAttr _ -> nt

(* Some parts of the pipeline compare nonterminal names case-insensitively *)
let rec lowercase = function
| User s -> User (String.lowercase_ascii s)
| SynthAttr attr -> SynthAttr (String.lowercase_ascii attr)
| InhAttr (nt, attr) -> InhAttr (String.lowercase_ascii nt, String.lowercase_ascii attr)
| Stub stub -> Stub { stub with stands_for = lowercase stub.stands_for }

let equal_ci nt1 nt2 = equal (lowercase nt1) (lowercase nt2)

(* Generated nonterminals holding attributes (not part of Goblin's output) *)
let is_attribute = function
| SynthAttr _ | InhAttr _ -> true
| User _ | Stub _ -> false

(* Name of the corresponding SMT-LIB symbol (and other generated identifiers).
   Injective, since `%` cannot appear in user identifiers *)
let rec pp_symbol: Format.formatter -> t -> unit
= fun ppf nt -> match nt with
| User s -> Format.pp_print_string ppf s
| SynthAttr attr -> Format.fprintf ppf "%%_%s" attr
| InhAttr (nt, attr) -> Format.fprintf ppf "%%_%s%%%s" nt attr
| Stub { id; stands_for } ->
  Format.fprintf ppf "%s" (String.uppercase_ascii (Format.asprintf "_stub%d_%a" id pp_symbol stands_for))

let to_symbol nt = Format.asprintf "%a" pp_symbol nt

(* User-facing name, as written in the input (without angle brackets for nonterminals) *)
let pp: Format.formatter -> t -> unit
= fun ppf nt -> match nt with
| User _ | Stub _ -> pp_symbol ppf nt
| SynthAttr attr | InhAttr (_, attr) -> Format.pp_print_string ppf attr

module Ord = struct
  type nonrec t = t
  let compare = compare
end

module Map = Map.Make(Ord)
module Set = Set.Make(Ord)

module StubMap = Stdlib.Map.Make(struct
  type t = stub
  let compare stub1 stub2 = Int.compare stub1.id stub2.id
end)
