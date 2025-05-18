module A = Ast

(* For AST element 'element', if it has a semantic constraint, see if some other 
   production rule also constraints the same nonterminal. If so, remove the semantic constraint(s) 
   from this element up the grammar to where it is first constrained. 

   But, also need to make sure other references to this element also constrain it. 

   Example:
  -- Both first prod rule and type annotation constrain B 
  A -> B C | P1(B)
  C -> B 
  B :: Int | P2(B)
  -->
  -- Bump up constraint to first prod rule, but also second prod rule 
  A -> B C | P1(B) ^ P2(B)
  C -> B | P2(B)
  B :: Int 
*)
let merge: A.ast -> A.element -> A.ast 
= fun ast element -> match element with 
  | A.TypeAnnotation (_, _, []) -> ast @ [element] 
  | A.TypeAnnotation (nt, ty, scs) -> 
    (* If a prior AST rule constrains the NT, we need to push up the constraints *)
    if A.ast_constrains_nt ast nt then 
      let _ = List.iter (fun sc -> match sc with 
      | A.SyGuSExpr _ -> ()
      | Dependency _ -> 
        Utils.crash "Unsupported case in merge. This grammar has some nonterminal that is a computed term (of the form <nt> <- expr) and is also referenced elsewhere in some constraint. This is currently unsupported, but will be supported in the future."
      ) scs in 
      let ast = List.fold_left (fun acc element -> match element with
      | A.TypeAnnotation _ -> acc @ [element]
      | A.ProdRule (nt2, rhss) -> 
        let rhss = List.map (fun rhs -> match rhs with 
        | A.StubbedRhs _ -> rhs 
        | A.Rhs (ges, scs2) -> 
          if List.exists (fun ge -> match ge with 
          | A.Nonterminal nt3 -> nt = nt3  
          | A.StubbedNonterminal _ -> false
          ) ges 
          then A.Rhs (ges, scs @ scs2) (* Push up the constraints *)
          else rhs
        ) rhss in 
        acc @ [A.ProdRule (nt2, rhss)]
      ) [] ast in 
      ast @ [A.TypeAnnotation (nt, ty, [])]
    (* If no overlapping constraint, no action is required *)
    else ast @ [element]
  | A.ProdRule (nt, rhss) -> 
    (* To find overlaps, look for semantic constraint pairs where 
        * One semantic constraint is in __this__ production rule and has a nonterminal of the form 
          <nt_1>...<nt_n>
        * Another semantic constraint is in a __previous__ production rule and has form 
          <nt_0>.<nt_1>...<nt_n>, where 
          <nt_0> is the LHS of __this__ production rule
      *)

    (* let constrained_nts = List.concat_map (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> []
    | Rhs (_, scs) -> List.concat_map A.get_nts_from_sc scs
    ) rhss in
    let module SS = Utils.StringSet in 
    let constrained_nts = SS.of_list constrained_nts in 
    
    let rhss, constraints_to_push_up = _ in 
    let ast = _ in 
    ast @ [A.ProdRule (nt, rhss)] *)

    (* If a prior AST rule constrains the NT, we need to push up the constraints *)
    let scs = List.concat_map (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> []
    | A.Rhs (_, scs) -> scs
    ) rhss in
    if A.ast_constrains_nt ast nt then 
      (* To push the scs up a level, we need to prepend a dot notation reference *)
      let scs = List.map (fun sc -> match sc with 
      | A.SyGuSExpr expr -> A.SyGuSExpr (A.prepend_nt_to_dot_exprs nt expr)
      | Dependency _ -> 
        Utils.crash "Unsupported case in merge. This grammar has some nonterminal that is a computed term (of the form <nt> <- expr) and is also referenced elsewhere in some constraint. This is currently unsupported, but will be supported in the future."
      ) scs in 
      let ast = List.fold_left (fun acc element -> match element with
      | A.TypeAnnotation _ -> acc @ [element]
      | A.ProdRule (nt2, rhss) -> 
        let rhss = List.map (fun rhs -> match rhs with 
        | A.StubbedRhs _ -> rhs 
        | A.Rhs (ges, scs2) -> 
          if List.exists (fun ge -> match ge with 
          | A.Nonterminal nt3 -> nt = nt3  
          | A.StubbedNonterminal _ -> false
          ) ges 
          then 
            A.Rhs (ges, scs @ scs2) (* Push up the constraints *)
          else rhs
        ) rhss in 
        acc @ [A.ProdRule (nt2, rhss)]
      ) [] ast in 
      let rhss = List.map (fun rhs -> match rhs with 
      | A.StubbedRhs _ -> rhs 
      | Rhs (ges, _) -> Rhs (ges, [])
      ) rhss in
      ast @ [A.ProdRule (nt, rhss)]
    (* If no overlapping constraint, no action is required *)
    else ast @ [element]

let detect: A.ast -> A.element -> bool
= fun ast element -> match element with 
  | A.TypeAnnotation (_, _, []) -> false 
  | A.TypeAnnotation (nt, _, _ :: _) -> 
    A.ast_constrains_nt ast nt 
  | A.ProdRule (nt, rhss) -> 
    let scs = List.concat_map (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> []
    | A.Rhs (_, scs) -> scs
    ) rhss in
    A.ast_constrains_nt ast nt && List.length scs > 0

let detect_overlapping_constraints: A.ast -> bool 
= fun ast -> 
  List.fold_left (fun acc element -> 
    acc || detect ast element
  ) false ast

let merge_overlapping_constraints: A.ast -> A.ast 
= fun ast ->
  List.fold_left merge [] ast
