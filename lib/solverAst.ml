(* Generated terms, and parsed SMT solver responses *)

type solver_ast =
(* Grammar term: nonterminal (with optional RHS and occurrence indices) and children *)
| Node of (Nt.t * int option * int option) * solver_ast list
(* Value of a symbolic terminal *)
| Leaf of Value.t
(* Part of the term that is computed separately (derived field or divide and conquer subproblem) *)
| StubLeaf of Nt.stub
(* SMT model: (SMT symbol, value) pairs. The symbols are the ones Goblin declared. *)
| Model of (string * Value.t) list
(* No term exists (the SMT solver or the search reported unsatisfiability) *)
| Infeasible

let pp_print_solver_ast: Format.formatter -> solver_ast -> unit
= fun ppf solver_ast ->
  let rec pp_print_solver_ast' ppf solver_ast = match solver_ast with
  | Node ((constructor, idx1, idx2), subterms) ->
    (* Don't include attributes in output *)
    let subterms = if !Flags.debug then subterms else List.filter (fun st -> match st with
    | Node ((constructor, _, _), _) -> not (Nt.is_attribute constructor)
    | Leaf _ | StubLeaf _ | Model _ | Infeasible -> true
    ) subterms in
    Format.fprintf ppf "(%a%a%a %a)"
    (if !Flags.debug then Nt.pp_symbol else Nt.pp) constructor
    (fun _ppf idx1 -> match idx1 with None -> () | Some idx1 -> Format.printf "@@{%d}" idx1) idx1
    (fun _ppf idx2 -> match idx2 with None -> () | Some idx2 -> Format.printf "[%d]" idx2) idx2
    (Lib.pp_print_list pp_print_solver_ast' " ") subterms
  | Leaf value -> Value.pp ppf value
  | StubLeaf stub -> Format.fprintf ppf "\"%s\"" (String.lowercase_ascii (Nt.to_symbol (Stub stub)))
  | Model values ->
    Format.fprintf ppf "(%a)"
      (Lib.pp_print_list (fun ppf (symbol, value) ->
        Format.fprintf ppf "(%s %a)" symbol Value.pp_smt value) " ") values
  | Infeasible -> Format.fprintf ppf "infeasible"
  in
  Format.fprintf ppf "%a\n"
  pp_print_solver_ast' solver_ast
