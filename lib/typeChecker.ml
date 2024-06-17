(*
  1. SyGuS exprs are type boolean 
  2. For dependency x <- y, y has same type as x   
  3. Other expression type checking stuff 
*)

open Ast

module StringMap = Map.Make(String)
type context = il_type StringMap.t

let build_context: ast -> ast * context
= fun ast -> 
  let ctx = List.fold_left (fun acc element -> match element with 
  | ProdRule _ -> acc 
  | TypeAnnotation (nt, ty, _) -> StringMap.add nt ty acc
  ) StringMap.empty ast in 
  (* Filter out type annotations with no semantic constraints. They are 
     no longer needed because their information is fully captured by 
     the type checking context. *)
  let ast = List.filter (fun element -> match element with 
  | TypeAnnotation (_, _, []) -> false
  | TypeAnnotation _ -> true
  | ProdRule _ -> true 
  ) ast in 
  ast, ctx


let check_type_expr: context -> SyntaxChecker.prod_rule_map -> string -> grammar_element list -> expr -> il_type -> expr 
= fun _ _ _ _ expr _ -> expr

let check_types: context -> SyntaxChecker.prod_rule_map -> ast -> ast 
= fun ctx prm ast -> 
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, ges, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) -> 
      let exp_ty = 
        match StringMap.find_opt nt2 ctx with 
        | None -> 
          failwith "Dependency LHS must be a nonterminal with a primitive (non-inductive) type."
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx prm nt ges expr exp_ty in 
      Dependency (nt2, expr)
    | SyGuSExpr expr -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx prm nt ges expr exp_ty in 
      SyGuSExpr expr
    ) scs in 
    ProdRule (nt, ges, scs)
  | TypeAnnotation (nt, ty, scs) -> 
    let scs = List.map (fun sc -> match sc with 
    | Dependency (nt2, expr) ->
      let exp_ty = 
        match StringMap.find_opt nt2 ctx with 
        | None -> 
          failwith "Dependency LHS must be a nonterminal with a primitive (non-inductive) type."
        | Some exp_ty -> exp_ty 
      in
      let expr = check_type_expr ctx prm nt [] expr exp_ty in 
      Dependency (nt2, expr) 
    | SyGuSExpr expr -> 
      let exp_ty = Bool in
      let expr = check_type_expr ctx prm nt [] expr exp_ty in 
      SyGuSExpr expr
    ) scs in 
    TypeAnnotation (nt, ty, scs)
  ) ast in 
  ast