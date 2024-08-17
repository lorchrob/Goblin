open Ast
open Graph


let rec from_ge_list_to_string_list (ge_list : grammar_element list) : string list = 
  match ge_list with
  | [] -> []  
  | Nonterminal(x)::xs -> x :: from_ge_list_to_string_list xs 
  | NamedNonterminal(x,y)::xs -> x :: y :: from_ge_list_to_string_list xs 
  | StubbedNonterminal(x,y)::xs -> x :: y :: from_ge_list_to_string_list xs 


let rec get_all_nt_from_rhs (rvalue : prod_rule_rhs list) : string list = 
  match rvalue with 
  | [] -> []
  | Rhs(ge_list, _)::xs -> (from_ge_list_to_string_list ge_list) @ (get_all_nt_from_rhs xs)  
  | StubbedRhs(_)::xs -> get_all_nt_from_rhs xs 

let get_all_dependencies_from_one_element (ge : element) : (string * string) list = 
  match ge with 
  | ProdRule(lvalue, rhs) -> (List.map (fun x-> (lvalue, x))(get_all_nt_from_rhs rhs))  |> (List.filter (fun (x,y) ->  x <> y) )
  | TypeAnnotation(_, _, _) -> [] 

let rec get_all_dependencies_from_grammar (g : ast) : (string * string) list = 
  match g with 
  | [] -> [] 
  | x :: xs -> get_all_dependencies_from_one_element x @ get_all_dependencies_from_grammar xs   

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

module G = Imperative.Digraph.Concrete(Node)

module StringPairSet = Set.Make(
  struct type t = string * string 
let compare (s11, s12) (s21, s22) = 
  match Stdlib.compare s11 s21 with 
  | 0 -> Stdlib.compare s12 s22 
  | c -> c 
end
) 

module StringSet = Set.Make(
  struct type t = string 
let compare = Stdlib.compare 
end
)


let rec create_set_from_list myset  (dependency_list : (string * string) list) = 
  match dependency_list with 
  | [] -> myset 
  | x::xs -> create_set_from_list (StringPairSet.add x myset) xs 

let print_tuple_list lst =
  List.iter (fun (x, y) -> Printf.printf "(%s, %s)\n" x y) lst;;

let print_list lst =
  List.iter (fun x -> Printf.printf "%s " x) lst;
  print_endline "" ;;
(* 
let remove_duplicates x = 
  let d_set = StringPairSet.of_list x in 
   
  StringPairSet.iter (fun s -> G.add_vertex g s) d_set ;
  StringPairSet.iter (fun s -> G.add_edge g (first s) (second s)) edge_pairs ;  *)
(* 
  let set_rep = create_set_from_list StringPairSet.empty x in 
  StringPairSet.to_list set_rep     *)


let rec collect_rules_for_nt (cnt : string) (ogrammar : ast) : ast = 
  match ogrammar with 
  | [] -> [] 
  | ProdRule(x, y):: xs -> 
    if x = cnt then ProdRule(x, y):: collect_rules_for_nt cnt xs
    else collect_rules_for_nt cnt xs 
    | TypeAnnotation(x, y, z)::xs -> 
      if x = cnt then TypeAnnotation(x, y, z)  :: collect_rules_for_nt cnt xs
      else collect_rules_for_nt cnt xs 

let rec collect_rules (non_term_list : string list ) (ogrammar : ast) (cgrammar : ast) : ast = 
  match non_term_list with 
  | [] -> cgrammar 
  | x::xs -> 
    let cntr = collect_rules_for_nt x ogrammar in 
    collect_rules xs ogrammar (cgrammar @ cntr)  

let canonicalize (ogrammar : ast) : ast option = 
  let g = G.create() in 
  let all_nt = get_all_nt ogrammar in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_grammar ogrammar in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (first s) (second s)) unique_dependencies ;
  let module My_Dfs = Traverse.Dfs(G) in
  if (My_Dfs.has_cycle g) then None  
  else 
    let module TopSort = Topological.Make_stable(G)  in  
    TopSort.iter (fun x -> Printf.printf "%s " x) g;   Printf.printf "\n\n"; 
    let top_sort_nts = TopSort.fold (fun x y -> y @ [x]) g [] in 
    (* let rev_top_sort_nts = List.rev top_sort_nts in  *)
    Some(collect_rules top_sort_nts ogrammar [])

let find_vertex g label =
  let vertex = ref None in
  G.iter_vertex (fun v -> if G.V.label v = label then vertex := Some v) g;
  match !vertex with
  | Some v -> v
  | None -> failwith "Vertex not found"  (* This case should never happen as per the assumption *)
    
let dead_rule_removal (canonicalized_grammar : ast) (start_symbol : string) : ast option =
  let g = G.create() in
  let all_nt = get_all_nt canonicalized_grammar in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_grammar canonicalized_grammar in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (first s) (second s)) unique_dependencies ;
  let start = find_vertex g start_symbol in
  let module CheckPath = Path.Check(G) in
  let path_checker = CheckPath.create g in
  let connected_paths = StringSet.filter (fun x -> CheckPath.check_path path_checker start (find_vertex g x)) unique_nts in
  let dead_rule_removed_graph = collect_rules (StringSet.fold (fun x y -> x :: y) connected_paths []) canonicalized_grammar [] in
  canonicalize dead_rule_removed_graph ;


  
(* 
let buildGraph (gr : ast) =
  let g = G.create() in 
  let all_nt = get_all_nt gr in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_grammar gr in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (first s) (second s)) unique_dependencies ;

  G.iter_vertex (fun x->print_endline x) g; 
  Printf.printf "\n\n\n\n\n\n" ;
  G.iter_edges_e (fun edg -> Printf.printf "(%s -> %s)\n" (first edg) (second edg)) g ; 
  Printf.printf "\n\n\n\n\n\n" ;
  G.iter_edges_e (fun edg -> if G.is_directed then Printf.printf "(%s -> %s)\n" (first edg) (second edg) else Printf.printf "skipping edge (%s %s)\n" (first edg) (second edg)) g ; 
  Printf.printf "\n\n\n\n\n\n" ;
  let module My_Dfs = Traverse.Dfs(G) in 
  if (My_Dfs.has_cycle g) then Printf.printf "Graph has cycles\n" 
  else Printf.printf "Graph does not have cycles\n"; 
  let module TopSort = Topological.Make_stable(G)  in  
    TopSort.iter (fun x -> Printf.printf "%s " x) g;   Printf.printf "\n\n"; 
    let top_sort_nts = TopSort.fold (fun x y -> x :: y) g [] in 
    let rev_top_sort_nts = List.rev top_sort_nts in 
    List.iter (fun x-> Printf.printf "%s " x) top_sort_nts; Printf.printf "\n\n";
    List.iter (fun x-> Printf.printf "%s " x) rev_top_sort_nts; 
    Printf.printf "\n"; 
  g
(*  *)

  print_tuple_list (remove_duplicates (get_all_dependencies_from_grammar gr)); 
  Printf.printf "\n\n\n\n\n\n" ;
  let g = G.create () in
  let allNt = get_all_nt gr in
  print_list allNt ;
  let all_dependencies = get_nt_dependency_pairs allNt gr in
  let edge_pairs = get_edge_pairs all_dependencies in
  print_tuple_list edge_pairs ;
  List.iter (fun s -> G.add_vertex g s) allNt ;
  List.iter (fun s -> G.add_edge g (first s) (second s)) edge_pairs ; *)
  (* g *)


(* let runGraph g =
  let _ = buildGraph g in

  () *)