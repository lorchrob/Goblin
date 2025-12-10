module A = Ast

let check_element attribute_ctx ctx element = match element with 
| A.TypeAnnotation (_, _, scs, p) -> 
  let scs = List.filter (fun sc -> match sc with 
  | A.AttrDef _ -> true 
  | _ -> false 
  ) scs in 
  if List.length scs > 0 then 
    let msg = "Synthesized attribute definitions are not allowed in type annotations" in 
    Utils.error msg p
  else element
| A.ProdRule (nt, _, rhss, p) -> 
  (* Check all inherited attribute "calls" are type-correct and arity-correct 
     (also covering the "default" case where nothing is passed *)
  let rhss = List.map (fun rhs -> match rhs with 
  | A.StubbedRhs _ -> rhs 
  | A.Rhs (ges, _, _, _) -> 
    let _ = List.map (fun ge -> match ge with 
    | A.StubbedNonterminal _ -> ge 
    | A.Nonterminal (nt, _, ias, p) -> 
      let arg_tys = match Utils.StringMap.find_opt nt attribute_ctx with 
      | Some arg_tys -> arg_tys 
      | None -> [] in 
      if List.length arg_tys <> List.length ias then (
        let msg = 
          Format.asprintf "Nonterminal %s is passed the incorrect number of inherited attributes (found %d, expected %d)" 
            nt (List.length ias) (List.length arg_tys)
        in  
        Utils.error msg p
      );
      let inf_tys = List.map (TypeChecker.infer_type_expr ctx SyGuS) ias 
        |> List.map Option.get in
      let type_correct = List.for_all2 A.eq_il_type arg_tys inf_tys in
      if not type_correct then (
        let inf_ty, exp_ty = List.find (fun (inf_ty, arg_ty) -> 
          not (A.eq_il_type inf_ty arg_ty)
        ) (List.combine inf_tys arg_tys) in
        let msg = Format.asprintf "Inherited attribute passed to nonterminal %s has expected type %a but inferred type %a" 
          nt 
          A.pp_print_ty exp_ty 
          A.pp_print_ty inf_ty in 
        Utils.error msg p
      );
      ge
    ) ges in 
    rhs
  ) rhss in 
  let synth_attrss = List.map (fun rhs -> match rhs with 
  | A.StubbedRhs _ -> [] 
  | A.Rhs (_, scs, _, _) -> 
    List.filter_map (fun sc -> match sc with 
    | A.AttrDef (attr, _, _) -> Some attr
    | _ -> None 
    ) scs
  ) rhss in 
  (* Check no duplicate synth attribute definitions (within an RHS) *) 
  let duplicate_attribute_definition = List.exists (Utils.has_duplicate String.equal) synth_attrss in 
  if duplicate_attribute_definition then (
    let msg = Format.asprintf "Nonterminal %s has some synthesized attribute with duplicate definitions (within a given production rule option)" nt in 
    Utils.error msg p
  );  
  let str_list_eq l1 l2 = 
    List.length l1 = List.length l2 && 
    List.for_all2 String.equal l1 l2 
  in
  (* Check each rhs has exactly the same set of synthesized attributes *)
  if Utils.all_equal synth_attrss str_list_eq then 
    element 
  else 
    let msg = Format.asprintf "Nonterminal %s has some synthesized attribute that is not defined in every production rule" nt in 
    Utils.error msg p

let check_attributes ctx ast = 
  (* Map from nonterminals to inferred attribute arg types *)
  let attribute_ctx = List.fold_left (fun acc element -> match element with 
  | A.TypeAnnotation _ -> acc 
  | A.ProdRule (nt, ias, _, _) -> 
    let tys = List.map (fun ia -> 
      let ia = "%_" ^ ia in 
      Utils.StringMap.find ia ctx
    ) ias in 
    Utils.StringMap.add nt tys acc 
  ) Utils.StringMap.empty ast in
  List.map (check_element attribute_ctx ctx) ast
