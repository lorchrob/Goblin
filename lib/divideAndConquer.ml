(* open Ast *)

(* let split_ast: ast -> ast list 
= fun ast -> [ast] *)

(* Need grammar element map of nonterminal to its set of grammar elements. 
   Then given an input AST, we compute a condensed AST that only includes the elements 
   takes from chasing the grammar element map. If an element with a semantic constraint 
   is encountered, we include a stub element in the computed condensed AST, and list 
   this element as the head of another subproblem *)
(* let rec stub_subproblems: element -> ast * element list
= fun element -> match element with 
| ProdRule (_, ges, []) ->
  let elements = List.map find_elements ges |> List.flatten in 
  let rec_asts, rec_elements = List.map stub_subproblems elements |> List.split in
  let rec_asts = List.flatten rec_asts in
  let rec_elements = List.flatten rec_elements in 
  element :: rec_asts, rec_elements
| ProdRule _
| TypeAnnotation _ -> assert false 

let rec split_element: element -> ast list 
= fun element -> 
  let new_ast, subproblems = stub_subproblems element in 
  new_ast :: (List.map split_element subproblems |> List.flatten)

let split_ast: ast -> ast list 
= fun ast -> match ast with 
| [] -> [] 
| element :: _ -> split_element element *)

let split_ast ast = [ast]