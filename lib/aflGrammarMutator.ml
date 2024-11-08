open Ast

let create_nonterm_map (ast : ast) : (string, prod_rule_rhs list) Hashtbl.t =
  let nonterm_map = Hashtbl.create 10 in
  let add_production name rhs =
    if Hashtbl.mem nonterm_map name then
      let existing_rhs = Hashtbl.find nonterm_map name in
      Hashtbl.replace nonterm_map name (rhs :: existing_rhs)
    else
      Hashtbl.add nonterm_map name [rhs]
  in
  let rec traverse_ast = function
    | [] -> ()
    | ProdRule (name, rhs_list) :: rest ->
        List.iter (fun rhs -> add_production name rhs) rhs_list;
        traverse_ast rest
    | TypeAnnotation (_, _) :: rest -> traverse_ast rest
  in
  traverse_ast ast;
  nonterm_map


let rule_based_mutation (ast : ast) (nonterm_map : (string, prod_rule_rhs list) Hashtbl.t) : ast =
  let rec mutate_rule = function
    | ProdRule (name, rhs_list) ->
        let mutated_rhs = List.map mutate_rhs rhs_list in
        ProdRule (name, mutated_rhs)
    | TypeAnnotation (name, typ) -> TypeAnnotation (name, typ)
  and mutate_rhs = function
    | Rhs elements -> 
        List.map (function
          | Nonterminal nt -> 
              (match Hashtbl.find_opt nonterm_map nt with
               | Some alternatives -> List.nth alternatives (Random.int (List.length alternatives))
               | None -> Nonterminal nt)
          | element -> element) elements
    | StubbedRhs s -> StubbedRhs s
  in
  List.map mutate_rule ast

let random_mutation (ast : ast) : ast =
  let randomize element = 
    match element with
    | Nonterminal nt -> if bool () then Nonterminal nt else StubbedNonterminal (nt, "random")
    | NamedNonterminal (name, nt) -> if bool () then NamedNonterminal (name, nt) else Nonterminal nt
    | other -> other
  in
  let rec mutate_rule = function
    | ProdRule (name, rhs_list) -> 
        ProdRule (name, List.map (function Rhs elements -> Rhs (List.map randomize elements) | x -> x) rhs_list)
    | other -> other
  in
  List.map mutate_rule ast

let recursive_mutation (ast : ast) (max_depth : int) : ast =
  let rec expand element depth = 
    match element with
    | Nonterminal nt when depth < max_depth -> Nonterminal nt
    | element -> element
  in
  let mutate_rhs rhs depth = 
    match rhs with
    | Rhs elements -> Rhs (List.map (fun e -> expand e depth) elements)
    | StubbedRhs s -> StubbedRhs s
  in
  let rec mutate_rule rule depth =
    match rule with
    | ProdRule (name, rhs_list) -> ProdRule (name, List.map (fun rhs -> mutate_rhs rhs depth) rhs_list)
    | other -> other
  in
  List.map (fun r -> mutate_rule r 0) ast

let splicing_mutation (ast1 : ast) (ast2 : ast) : ast =
  let splice_elements elems1 elems2 = 
    let rec combine e1 e2 =
      match e1, e2 with
      | Nonterminal n1, Nonterminal n2 -> Nonterminal (if bool () then n1 else n2)
      | NamedNonterminal (name, n1), NamedNonterminal (_, n2) -> NamedNonterminal (name, if bool () then n1 else n2)
      | x, _ -> x
    in
    List.map2 combine elems1 elems2
  in
  let rec splice_rules r1 r2 =
    match r1, r2 with
    | ProdRule (name1, rhs_list1), ProdRule (_, rhs_list2) -> 
        ProdRule (name1, List.map2 splice_elements rhs_list1 rhs_list2)
    | other, _ -> other
  in
  List.map2 splice_rules ast1 ast2

let subtree_trimming (ast : ast) : ast =
  let trim_rhs = function
    | Rhs elements -> Rhs (List.filter (fun _ -> bool ()) elements)
    | other -> other
  in
  let rec trim_rule = function
    | ProdRule (name, rhs_list) -> ProdRule (name, List.map trim_rhs rhs_list)
    | other -> other
  in
  List.map trim_rule ast

let recursive_trimming (ast : ast) (depth : int) : ast =
  if depth = 0 then ast else
    let trim_rhs rhs = 
      match rhs with
      | Rhs elements -> Rhs (List.filter (fun _ -> bool ()) elements)
      | other -> other
    in
    let trim_rule rule =
      match rule with
      | ProdRule (name, rhs_list) -> ProdRule (name, List.map trim_rhs rhs_list)
      | other -> other
    in
    recursive_trimming (List.map trim_rule ast) (depth - 1)

(* Main mutation selection function *)

let mutate_ast (ast : ast) : ast =
  let nonterm_map = create_nonterm_map ast in
  let mutation_type = Random.int 6 in
  match mutation_type with
  | 0 -> rule_based_mutation ast nonterm_map
  | 1 -> random_mutation ast
  | 2 -> recursive_mutation ast 3
  | 3 -> if bool () then splicing_mutation ast (List.rev ast) else splicing_mutation ast ast
  | 4 -> subtree_trimming ast
  | _ -> recursive_trimming ast 3

