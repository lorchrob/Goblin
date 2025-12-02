module A = Ast

let eliminate_ta_constraints full_ast = 
  let rec helper full_ast ast = match ast with 
  | [] -> full_ast 
  | (A.ProdRule _) :: tl -> 
    helper full_ast tl   
  | A.TypeAnnotation (nt, _, scs, _) :: tl -> 
    let full_ast = List.map (function 
    | A.TypeAnnotation (nt2, ty, _, p) as element ->
      if nt = nt2 then A.TypeAnnotation (nt2, ty, [], p) else element
    | A.ProdRule (nt3, ias, rhss, pos2) -> 
      let rhss = List.map (function 
        | A.StubbedRhs _ as rhs -> rhs 
        | Rhs (ges, scs2, prob, pos3) -> 
          let contains_nt = List.exists (fun ge -> match ge with 
          | A.StubbedNonterminal _ -> false 
          | A.Nonterminal (nt2, _, _, _) -> nt = nt2
          ) ges in 
          if contains_nt then 
            Rhs (ges, scs2 @ scs, prob, pos3)
          else Rhs (ges, scs2, prob, pos3)
      ) rhss in 
      ProdRule (nt3, ias, rhss, pos2) 
    ) full_ast in 
    helper full_ast tl 
  in 
  helper full_ast full_ast 
