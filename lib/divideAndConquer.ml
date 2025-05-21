open Ast

(* Loop through all production rules and type annotations. 
   If we hit a semantic constraint, stub it out, 
   make a recursive call to generate a new AST 
   with new top-level element, and continue. *)
  
let rec stub_subproblems_prod_rule_rhss
= fun nt elements rhss -> match rhss with 
| [] -> [], []
| (Rhs (_, []) as hd) :: tl 
| (StubbedRhs _ as hd) :: tl -> 
  let tl', subproblems = stub_subproblems_prod_rule_rhss nt elements tl in
  hd :: tl', subproblems 
| Rhs (ges, scs) :: tl -> 
  let tl', subproblems = stub_subproblems_prod_rule_rhss nt elements tl in
  let stub_id = Utils.mk_fresh_stub_id nt in
  StubbedRhs (stub_id) :: tl', (ProdRule (stub_id, [Rhs (ges, scs)]) :: elements) :: subproblems

let stub_subproblems: ast -> ast * ast list
= fun ast -> 
  let rec stub_subproblems' ast = match ast with 
  | element :: elements -> (match element with 
    | ProdRule (nt, rhss) ->
      (* NOTE: We could consider all the RHSs together rather than splitting them up. Then the solver 
         can decide which production rule to use, rather than doing all of them. *)
      let ast', subproblems1 = stub_subproblems' elements in 
      let rhss, subproblems2 = stub_subproblems_prod_rule_rhss nt elements rhss in
      ProdRule (nt, rhss) :: ast', subproblems1 @ subproblems2
    | TypeAnnotation (_, _, []) -> 
      let ast', subproblems = stub_subproblems' elements in 
      element :: ast', subproblems
    | TypeAnnotation (nt, ty, scs) ->
      let ast', subproblems = stub_subproblems' elements in 
      ProdRule (nt, [StubbedRhs nt]) :: ast', (TypeAnnotation (nt, ty, scs) :: elements) :: subproblems
    )
  | [] -> [], []
  in 
  (* The start symbol's semantic constraints should not be stubbed *)
  match ast with 
  | [] -> [], [] 
  | element :: ast -> 
    let ast, subproblems = stub_subproblems' ast in 
    element :: ast, subproblems

let rec split_ast': ast -> ast list 
= fun ast -> 
    let new_ast, subproblems = stub_subproblems ast in 
    new_ast :: (List.map split_ast' subproblems |> List.flatten)
    |> List.filter (fun ast -> ast <> [])

let split_ast: ast -> ast list option
= fun ast -> 
  let asts = split_ast' ast in 
  let asts = List.map (fun ast -> match ast with 
    | ProdRule (sym, _) :: _-> TopologicalSort.dead_rule_removal ast sym 
    | TypeAnnotation (sym, ges, scs) :: _ -> Some [TypeAnnotation (sym, ges, scs)]
    | _ -> Utils.crash "Unexpected case in split_ast"
  ) asts in 
  Utils.sequence_option asts