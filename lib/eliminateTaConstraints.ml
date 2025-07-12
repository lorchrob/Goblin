module A = Ast

let eliminate_ta_constraints full_ast = 
  let rec helper ast_prefix ast = match ast with 
  | [] -> ast_prefix 
  | (A.ProdRule _ as hd) :: tl -> 
    helper (ast_prefix @ [hd]) tl   
  | A.TypeAnnotation (nt, ty, scs) :: tl -> 
    let ast_prefix = List.map (function 
    | A.TypeAnnotation _ as element -> element  
    | A.ProdRule (nt3, rhss) -> 
      let rhss = List.map (function 
        | A.StubbedRhs _ as rhs -> rhs 
        | Rhs (ges, scs2) -> 
          let contains_nt = List.exists (fun ge -> match ge with 
          | A.StubbedNonterminal _ -> false 
          | A.Nonterminal (nt2, _) -> nt = nt2
          ) ges in 
          if contains_nt then 
            Rhs (ges, scs2 @ scs)
          else Rhs (ges, scs2)
      ) rhss in 
      ProdRule (nt3, rhss) 
    ) ast_prefix in 
    helper (ast_prefix @ [TypeAnnotation (nt, ty, [])]) tl 
  in 
  helper [] full_ast 
