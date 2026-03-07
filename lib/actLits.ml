module A = Ast

let handle_sc sc =  
  match sc with 
  | A.DerivedField _ -> sc 
  | A.SmtConstraint (expr, p) -> 
    let nt_exprs = A.get_nts_from_expr2 expr in  
    let act_lits = List.map (fun nt_expr -> 
      A.ActLit (NTExpr (nt_expr, p), p) 
    ) nt_exprs in
    let expr = 
      if act_lits = [] then expr 
      else 
        let all_act_lits = List.fold_left (fun acc act_lit -> 
          A.BinOp (acc, A.LAnd, act_lit, p)  
        ) (List.hd act_lits) (List.tl act_lits) in 
        A.BinOp (all_act_lits, A.LImplies, expr, p) 
    in 
    A.SmtConstraint (expr, p) 
  | A.AttrDef _ -> assert false

let handle_rhs rhs = 
  match rhs with 
  | A.StubbedRhs _ -> rhs 
  | A.Rhs (ges, scs, pr, p) -> 
    let scs = List.map handle_sc scs in 
    A.Rhs (ges, scs, pr, p)

let add_act_lits ast = 
  List.map (function 
  | A.ProdRule (nt, ias, rhss, p) -> 
    let rhss = List.map handle_rhs rhss in 
    A.ProdRule (nt, ias, rhss, p)
  | A.TypeAnnotation _ as element -> element
  ) ast
