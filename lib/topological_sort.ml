open Ast
open Graph

let rec get_nt_from_geList geList = 
  match geList with
  | [] -> []
  | Nonterminal(x) :: xs -> x :: (get_nt_from_geList xs)
  | NamedNonterminal(_,_) :: xs | StubbedNonterminal(_,_) :: xs -> (get_nt_from_geList xs)
 
let rec get_nt_from_rhs rhs =
  match rhs with
  | [] -> []
  | Rhs(geList, _) :: xs -> (get_nt_from_geList geList) @ (get_nt_from_rhs xs)
  | StubbedRhs(_) :: xs -> (get_nt_from_rhs xs)

let rec get_all_nt (g : ast) : string list =
  match g with
  | [] -> []
  | ProdRule(nt, rhs) :: xs -> nt :: (get_nt_from_rhs rhs)  @ (get_all_nt xs)
  | TypeAnnotation(_, _, _) :: xs -> get_all_nt xs

let rec get_dependencies (nt : string) (geList : grammar_element list) : string list =
  match geList with
  | [] -> []
  | Nonterminal(x) :: xs -> 
    if nt = x 
      then x :: (get_dependencies nt xs)
    else get_dependencies nt xs
  | _ :: xs -> get_dependencies nt xs

let rec get_all_rhs_elements (nt : string) (prList : prod_rule_rhs list) : string list =
  match prList with
  | [] -> []
  | Rhs(geList, _) :: xs -> (get_dependencies nt geList) @ (get_all_rhs_elements nt xs)
  | StubbedRhs(_) :: xs -> get_all_rhs_elements nt xs

let first (tuple: ('a * 'b)) : 'a =
  match tuple with
  (t1, _) -> t1
;;

let second (tuple: ('a * 'b)) : 'b =
  match tuple with
  (_, t2) -> t2
;;
  
let rec get_edge_pairs (nts : (string * (string list)) list): (string * string) list =
  match nts with
  | [] -> []
  | (a, xs) :: ys -> (List.map (fun x -> (a, x)) xs) @ (get_edge_pairs ys)

let rec get_all_rules (nt : string) (g : ast) : prod_rule_rhs list =
  match g with
  | [] -> []
  | ProdRule(a, prod_rule_lst) :: xs ->
    if a = nt
      then prod_rule_lst @ (get_all_rules nt xs)
    else
      get_all_rules nt xs
  | TypeAnnotation(_, _, _) :: xs -> (get_all_rules nt xs)

let rec get_nt_dependency_pairs (nts : string list) (g : ast) : (string * (string list)) list =
  match nts with
  | [] -> []
  | x :: xs -> (x, (get_all_rhs_elements x (get_all_rules x g))) :: (get_nt_dependency_pairs xs g)

module Node = struct                                      
  type t = string                                                                     
  let compare = Stdlib.compare                                                 
  let hash = Hashtbl.hash                                                          
  let equal = (=)                                                                  
end                                                                                 
(* 
module Edge = struct                                                                
  type t = string                                                                  
  let compare = Pervasives.compare                                                 
  let equal = (=)                                                                  
  let default = ""                                                                 
end *)

module G = Imperative.Digraph.ConcreteBidirectional(Node)

let print_tuple_list lst =
  List.iter (fun (x, y) -> Printf.printf "(%s, %s)\n" x y) lst;;

let print_list lst =
  List.iter (fun x -> Printf.printf "%s " x) lst;
  print_endline "" ;;

let buildGraph gr =
  let g = G.create () in
  let allNt = get_all_nt gr in
  print_list allNt ;
  let all_dependencies = get_nt_dependency_pairs allNt gr in
  let edge_pairs = get_edge_pairs all_dependencies in
  print_tuple_list edge_pairs ;
  List.iter (fun s -> G.add_vertex g s) allNt ;
  List.iter (fun s -> G.add_edge g (first s) (second s)) edge_pairs ;
  g

let runGraph grammar =
  let _ = buildGraph grammar in
  ()