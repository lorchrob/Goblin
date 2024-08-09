open Ast
open Graph

let rec get_all_nt (g : ast) : grammar_element list =
  match g with
  | [] -> []
  | ProdRule(nt, _) :: xs -> nt :: (get_all_nt xs)
  | TypeAnnotation(nt, _, _) :: xs -> get_all_nt xs

let rec get_dependencies (nt : string) (geList : grammar_element list) : string list =
  match geList with
  | [] -> []
  | Nonterminal(x) :: xs -> 
    if nt = x 
      then x :: (get_dependencies xs)
    else get_dependencies nt xs
  | _ :: xs -> get_dependencies nt xs

let rec get_all_rhs_elements (nt : string) (prList : prod_rule_rhs list) : string list =
  match prList with
  | [] -> []
  | Rhs(geList, _) :: xs -> (get_dependencies nt geList) :: (get_all_rhs_elements xs)
  | StubbedRhs(_,_) :: xs -> get_all_rhs_elements nt xs

let get_edge_pairs (nt : string) (nt_list : string list) : (string * string) list =
  List.map (fun y -> (nt, y)) nt_list

let rec get_all_rules (nt : string) (g : ast) : prod_rule_rhs list =
  match g with
  | [] -> []
  | ProdRule(a, prod_rule_lst) :: xs ->
    if a = nt
      then prod_rule_lst :: (get_all_rules nt xs)
    else
      get_all_rules nt xs
  | TypeAnnotation(a, b, c) :: xs -> 
    if a = nt then TypeAnnotation(a, b, c) :: (get_all_rules nt x)
    else get_all_rules nt xs

module G = Imperative.Digraph.ConcreteBidrectional(String)


let buildGraph gr =
  let g = G.create () in
  let allNt = get_all_nt gr in
  let all_dependencies = List.map (fun x -> get_all_rhs_elements x (get_all_rules x gr)) all_Nt in
  let edge_pairs = List.map (fun x -> (get_edge_pairs x all_dependencies)) allNt in
  pp_print_list edge_pairs ; 
  List.iter (fun s -> G.add_vertex g s) all_Nt ;
  List.iter (fun src dst -> G.add_edge g src dst) edge_pairs ;
  g