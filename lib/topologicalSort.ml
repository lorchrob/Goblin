open Ast
open Graph

let rec from_ge_list_to_string_list (ge_list : grammar_element list) : string list = 
  match ge_list with
  | [] -> []  
  | Nonterminal(x)::xs -> x :: from_ge_list_to_string_list xs 
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
  | StubbedNonterminal _ :: xs -> (get_nt_from_geList xs)
 
let rec get_nt_from_rhs rhs =
  match rhs with
  | [] -> []
  | Rhs (geList, _) :: xs -> (get_nt_from_geList geList) @ (get_nt_from_rhs xs)
  | StubbedRhs _ :: xs -> (get_nt_from_rhs xs)

let rec get_all_nt (g : ast) : string list =
  match g with
  | [] -> []
  | ProdRule (nt, rhs) :: xs -> nt :: (get_nt_from_rhs rhs)  @ (get_all_nt xs)
  | TypeAnnotation (_, _, _) :: xs -> get_all_nt xs

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
  | Rhs (geList, _) :: xs -> (get_dependencies nt geList) @ (get_all_rhs_elements nt xs)
  | StubbedRhs (_) :: xs -> get_all_rhs_elements nt xs

let rec get_edge_pairs (nts : (string * (string list)) list): (string * string) list =
  match nts with
  | [] -> []
  | (a, xs) :: ys -> (List.map (fun x -> (a, x)) xs) @ (get_edge_pairs ys)

let rec get_all_rules (nt : string) (g : ast) : prod_rule_rhs list =
  match g with
  | [] -> []
  | ProdRule (a, prod_rule_lst) :: xs ->
    if a = nt
      then prod_rule_lst @ (get_all_rules nt xs)
    else
      get_all_rules nt xs
  | TypeAnnotation (_, _, _) :: xs -> (get_all_rules nt xs)

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
  StringPairSet.iter (fun s -> G.add_edge g (fst s) (snd s)) edge_pairs ;  *)
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
  let g = G.create () in 
  let all_nt = get_all_nt ogrammar in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_grammar ogrammar in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (fst s) (snd s)) unique_dependencies ;
  let module MyDfs = Traverse.Dfs(G) in
  if (MyDfs.has_cycle g) then None  
  else 
    let module TopSort = Topological.Make_stable(G)  in  
    let top_sort_nts = TopSort.fold (fun x y -> y @ [x]) g [] in 
    (* let rev_top_sort_nts = List.rev top_sort_nts in  *)
    Some (collect_rules top_sort_nts ogrammar [])

let get_all_nt_scs scs = 
  List.fold_left (fun acc sc -> match sc with 
  | Dependency (nt, _) -> nt :: acc
  | SyGuSExpr _ -> acc
  ) [] scs

let get_all_dependencies_from_scs scs = 
  List.fold_left (fun acc sc -> match sc with 
  | SyGuSExpr _ -> acc
  | Dependency (nt1, expr) -> 
    let nts = Ast.get_nts_from_expr expr in 
    acc @ List.map (fun nt2 -> (nt1, nt2)) nts
  ) [] scs


let has_cycle g =
  let module H = Hashtbl.Make(G.V) in
  let h = H.create 97 in
  let stack = Stack.create () in
  let loop () =
    while not (Stack.is_empty stack) do
      let v = Stack.top stack in
      if H.mem h v then begin
        (* we are now done with node v *)
        (* assert (H.find h v = true); *)
        H.replace h v false;
        ignore (Stack.pop stack)
      end else begin
        (* we start DFS from node v *)
        H.add h v true;
        G.iter_succ
          (fun w ->
              try if H.find h w then raise Exit
              with Not_found -> Stack.push w stack)
          g v;
      end
    done
  in
  try
    G.iter_vertex
      (fun v ->
          if not (H.mem h v) then begin Stack.push v stack; loop () end)
      g;
    false
  with Exit ->
    true


(* Altered version of My_Dfs.has_cycle which returns the cycle *)
let find_cycle g =
  let module H = Hashtbl.Make(G.V) in
  let h = H.create 97 in
  let parent = H.create 97 in
  let stack = Stack.create () in
  let cycle_ref = ref None in (* Store the detected cycle *)

  let rec extract_cycle v start acc =
    if v = start then start :: acc
    else match H.find_opt parent v with
          | Some p -> extract_cycle p start (v :: acc)
          | None -> acc
  in

  let loop () =
    while not (Stack.is_empty stack) && !cycle_ref = None do
      let v = Stack.top stack in
      if H.mem h v then begin
        (* Mark node as fully processed *)
        H.replace h v false;
        ignore (Stack.pop stack)
      end else begin
        (* Start DFS from node v *)
        H.add h v true;
        G.iter_succ
          (fun w ->
              match H.find_opt h w with
              | Some true -> (* Found a back edge, cycle detected *)
                cycle_ref := Some (extract_cycle v w [w])
              | _ ->
                H.replace parent w v;
                Stack.push w stack
          ) g v
      end
    done
  in

  G.iter_vertex
    (fun v ->
        if not (H.mem h v) && !cycle_ref = None then begin
          Stack.push v stack;
          loop ()
        end)
    g;

  !cycle_ref


let canonicalize_scs (scs : semantic_constraint list) : string list option = 
  let g = G.create () in 
  let all_nt = get_all_nt_scs scs in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_scs scs in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (fst s) (snd s)) unique_dependencies ;
  match find_cycle g with 
  | Some cycle -> Some cycle 
  | None -> None

let find_vertex g label =
  let vertex = ref None in
  G.iter_vertex (fun v -> if G.V.label v = label then vertex := Some v) g;
  match !vertex with
  | Some v -> v
  | None -> Utils.crash "Vertex not found"  (* This case should never happen as per the assumption *)
    
let dead_rule_removal (canonicalized_grammar : ast) (start_symbol : string) : ast option =
  let g = G.create() in
  let all_nt = get_all_nt canonicalized_grammar in 
  let unique_nts = StringSet.of_list all_nt in 
  let all_dependencies = get_all_dependencies_from_grammar canonicalized_grammar in 
  let unique_dependencies = StringPairSet.of_list all_dependencies in 
  StringSet.iter (fun s -> G.add_vertex g s) unique_nts; 
  StringPairSet.iter (fun s-> G.add_edge g (fst s) (snd s)) unique_dependencies ;
  let start = find_vertex g start_symbol in
  let module CheckPath = Path.Check(G) in
  let path_checker = CheckPath.create g in
  let connected_paths = StringSet.filter (fun x -> CheckPath.check_path path_checker start (find_vertex g x)) unique_nts in
  let dead_rule_removed_graph = collect_rules (StringSet.fold (fun x y -> x :: y) connected_paths []) canonicalized_grammar [] in
  canonicalize dead_rule_removed_graph ;
