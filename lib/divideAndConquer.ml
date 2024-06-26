open Ast

(* Module state for creating fresh identifiers *)
let k = ref 0

(* Loop through all production rules and type annotations. 
   If we hit a semantic constraint, stub it out, 
   make a recursive call to generate a new AST 
   with new top-level element, and continue. *)
let mk_fresh_stub_id () = 
  let id = "_stub" ^ (string_of_int !k) ^ "_grammar_element" in 
  k := !k + 1;
  String.uppercase_ascii id
  

let stub_subproblems: ast -> ast * ast list
= fun ast -> 
  let rec stub_subproblems' ast = match ast with 
  | element :: elements -> (match element with 
    | ProdRule (_, _, []) 
    | TypeAnnotation (_, _, [])
    | StubbedElement _ -> 
      let ast, subproblems = stub_subproblems' elements in 
      element :: ast, subproblems
    | ProdRule (nt, ges, scs) ->
      let ast', subproblems = stub_subproblems' elements in 
      let stub_id = mk_fresh_stub_id () in
      StubbedElement (nt, stub_id) :: ast', (ProdRule (stub_id, ges, scs) :: elements) :: subproblems
    | TypeAnnotation (nt, ty, scs) ->
      let ast', subproblems = stub_subproblems' elements in 
      let stub_id = mk_fresh_stub_id () in
      StubbedElement (nt, stub_id) :: ast', (TypeAnnotation (stub_id, ty, scs) :: elements) :: subproblems
    )
  | [] -> [], []
  in 
  (* The start symbol's semantic constraints should not be stubbed *)
  match ast with 
  | [] -> [], [] 
  | element :: ast -> 
    let ast, subproblems = stub_subproblems' ast in 
    element :: ast, subproblems

let rec split_ast: ast -> ast list 
= fun ast -> 
    let new_ast, subproblems = stub_subproblems ast in 
    new_ast :: (List.map split_ast subproblems |> List.flatten)
    |> List.filter (fun ast -> ast <> [])

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

let split_ast: ast -> ast list 
= fun ast -> match ast with 
| [] -> [] 
| element :: _ -> split_element element
*)