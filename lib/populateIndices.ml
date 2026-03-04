module A = Ast

let disambiguate_nonterminals (rhss : A.prod_rule_rhs list) : A.prod_rule_rhs list =
  let running_indices = Hashtbl.create 10 in

  let disambiguate_elem idx1 = function
    | A.Nonterminal (name, _, _, ias, pos) ->
      let idx2 = Hashtbl.find_opt running_indices name |> Option.value ~default:0 in
      Hashtbl.replace running_indices name (idx2 + 1);
      A.Nonterminal (name, Some idx1, Some idx2, ias, pos)
    | other -> other
  in

  let disambiguate_rhs idx1 rhs = match rhs with 
    | A.StubbedRhs _ as stub -> stub
    | Rhs (elems, constraints, prob, pos) ->
      Hashtbl.clear running_indices;
      let new_elems = List.map (disambiguate_elem idx1) elems in
      Rhs (new_elems, constraints, prob, pos)
  in

  List.fold_left (fun (acc, acc_i) rhs -> 
    acc @ [disambiguate_rhs acc_i rhs], acc_i + 1
  ) ([], 0) rhss |> fst

let populate_indices ast = 
  List.map (fun element -> match element with 
  | A.TypeAnnotation _ -> element 
  | A.ProdRule (nt, ias, rhss, pos) -> 
    let rhss = disambiguate_nonterminals rhss in 
    ProdRule (nt, ias, rhss, pos)
  ) ast
