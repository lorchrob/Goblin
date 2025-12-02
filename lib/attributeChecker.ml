module A = Ast

let check_element _ctx element = match element with 
| A.TypeAnnotation _ -> element
| A.ProdRule (nt, _, rhss, p) -> 
  (* Check each rhs has exactly the same set of synthesized attributes *)
  let synth_attrss = List.map (fun rhs -> match rhs with 
  | A.StubbedRhs _ -> [] 
  | A.Rhs (_, scs, _, _) -> 
    List.filter_map (fun sc -> match sc with 
    | A.AttrDef (attr, _, _) -> Some attr
    | _ -> None 
    ) scs
  ) rhss in 
  let str_list_eq l1 l2 = 
    List.length l1 = List.length l2 && 
    List.for_all2 String.equal l1 l2 
  in
  if Utils.all_equal synth_attrss str_list_eq then 
    element 
  else 
    let msg = Format.asprintf "Nonterminal %s has some synthesized attribute that is not defined in every production rule" nt in 
    Utils.error msg p

let check_attributes ctx ast = 
  List.map (check_element ctx) ast
