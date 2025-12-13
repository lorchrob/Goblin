module A = Ast

let eliminate_ta_constraints full_ast = 
  let rec helper full_ast ast = match ast with 
  | [] -> full_ast 
  | (A.InlinedTypeProdRule _) :: _ -> assert false
  | (A.ProdRule _) :: tl -> 
    helper full_ast tl   
  | A.TypeAnnotation (nt, _, scs, _) :: tl -> 
    let attrs = List.filter (fun sc -> match sc with 
    | A.AttrDef _ -> true 
    | _ -> false 
    ) scs in
    let scs_no_attrs = List.filter (fun sc -> match sc with 
    | A.AttrDef _ -> false 
    | _ -> true 
    ) scs in
    let full_ast = List.map (function 
    | A.TypeAnnotation (nt2, ty, _, p) as element ->
      (* Maintain attribute definitions because we will detect and reject later *)
      if nt = nt2 then A.TypeAnnotation (nt2, ty, attrs, p) else element
    | A.InlinedTypeProdRule _ -> assert false
    | A.ProdRule (nt3, ias, rhss, pos2) -> 
      let rhss = List.map (function 
        | A.StubbedRhs _ as rhs -> rhs 
        | Rhs (ges, scs2, prob, pos3) -> 
          let contains_nt = List.exists (fun ge -> match ge with 
          | A.StubbedNonterminal _ -> false 
          | A.Nonterminal (nt2, _, _, _) -> nt = nt2
          ) ges in 
          if contains_nt then 
            (* Push up the scs not containing attribute definitions because we will detect 
               and reject type annotation attributes later *)
            Rhs (ges, scs2 @ scs_no_attrs, prob, pos3)
          else Rhs (ges, scs2, prob, pos3)
      ) rhss in 
      ProdRule (nt3, ias, rhss, pos2) 
    ) full_ast in 
    helper full_ast tl 
  in 
  helper full_ast full_ast 
