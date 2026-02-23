module A = Ast

let eliminate_ta_constraints full_ast = 
  let rec helper full_ast ast = match ast with 
  | [] -> full_ast 
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
    | A.ProdRule (nt3, ias, rhss, pos2) -> 
      let rhss, _ = List.fold_left (fun (acc, acc_count) rhs -> match rhs with
        | A.StubbedRhs _ as rhs -> acc @ [rhs], acc_count 
        | Rhs (ges, scs2, prob, pos3) -> 
          let acc_count = if List.exists (fun ge -> match ge with 
          | A.Nonterminal (nt2, _, _, _) -> nt = nt2
          | A.StubbedNonterminal _ -> false 
          ) ges then 
            acc_count + 1 
          else acc_count in
          let contains_nt = List.exists (fun ge -> match ge with 
          | A.StubbedNonterminal _ -> false 
          | A.Nonterminal (nt2, _, _, _) -> nt = nt2
          ) ges in 
          if contains_nt then (
            (* Push up the scs not containing attribute definitions because we will detect 
               and reject type annotation attributes later. 
               Also, save some work later by inserting the correct disambiguating index for these constraints. *)
            let scs_no_attrs = List.map (fun sc -> match sc with 
            | A.DerivedField _ 
            | AttrDef _ -> sc 
            | A.SmtConstraint (expr, pos) -> 
              SmtConstraint (A.add_index_to_expr (acc_count - 1) expr, pos)
            ) scs_no_attrs in
            acc @ [Rhs (ges, scs2 @ scs_no_attrs, prob, pos3)], acc_count
          ) else acc @ [Rhs (ges, scs2, prob, pos3)], acc_count
      ) ([], 0) rhss in 
      ProdRule (nt3, ias, rhss, pos2) 
    ) full_ast in 
    helper full_ast tl 
  in 
  helper full_ast full_ast 
