(*
  1. No dangling identifiers (no identifiers in RHS of some prod rule or in 
     semantic constraint that aren't on the LHS of some prod rule or have type annotation)
  2. All nonterminal expression dot notation chains are valid 
      a) NTExpr starts with a nonterminal on rule RHS if prod rule or rule LHS if type annotation 
      b) Each dot references a valid nonterminal
  3. No nonterminal has both a production rule and a type annotation

  (* TODO: For now, don't support redundant rule options (same list of NTs). 
       If we allow this, then match statements are a bit harder to generate, 
       because you could accidentally match the same constructor twice 
       rather than the distinct constructors. *)

  (* TODO: 
     Revisit dangling identifier checks (esp., for nonterminals on RHS not in constraints 

  *)
*)

open Ast


type prod_rule_map = (Nt.Set.t) Nt.Map.t

let (let*) = Res.(let*)

(* Build production rule map, which is a map from each grammar nonterminal 
   to the list of valid nonterminal references *)
let build_prm: ast -> prod_rule_map
= fun ast -> 
  let prm = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, _, rhss, p) -> 
    List.fold_left (fun acc rhss -> match rhss with 
    | Rhs (ges, _, _, _) -> 
      let grammar_elements = List.map Ast.nt_of_grammar_element ges in
      let grammar_elements = Nt.Set.of_list grammar_elements in (
      match Nt.Map.find_opt nt acc with 
      | Some mem -> 
        if (Nt.Set.is_empty mem) then 
          Utils.error (Format.asprintf "Nonterminal %a has both a type annotation and a production rule" Nt.pp nt) p
        else Nt.Map.add nt (Nt.Set.union mem grammar_elements) acc 
      | None -> 
        Nt.Map.add nt grammar_elements acc
      )
    | StubbedRhs _ -> acc
    ) acc rhss
  | TypeAnnotation (nt, _, _, p) -> 
    match Nt.Map.find_opt nt acc with 
    | Some _ -> 
      Utils.error (Format.asprintf "Nonterminal %a either has two type annotations, or has both a type annotation and a production rule" Nt.pp nt) p
    | None -> 
      Nt.Map.add nt Nt.Set.empty acc 
  ) Nt.Map.empty ast in 
  prm

(* Build nonterminal set, which the set of nonterminals with either
   their own production rules or type annotations *)
let build_nt_set: ast -> Nt.Set.t 
= fun ast -> 
  List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, _, _, _)
  | TypeAnnotation (nt, _, _, _) -> Nt.Set.add nt acc
  ) Nt.Set.empty ast

let rec check_dangling_identifiers: Nt.Set.t -> Lexing.position -> expr -> expr 
= fun nt_set p expr -> 
  let call = check_dangling_identifiers nt_set p in 
  let check_d_ids_nt_expr nt_expr = 
    List.iter (fun nt -> match Nt.Set.find_opt nt nt_set with 
    | None -> Utils.error (Format.asprintf "Dangling identifier <%a> (you are referencing a nonterminal which either does not exist or is not present in the current context)" Nt.pp nt) p
    | Some _ -> ()
    ) nt_expr
  in
  let check_d_ids_attribute attr = 
    match Nt.Set.find_opt (Nt.SynthAttr attr) nt_set with 
    | None -> Utils.error (Format.asprintf "Dangling identifier %s (you are trying to access an attribute that was never defined, or does not have a type annotation)" attr) p
    | Some _ -> ()
  in
  match expr with 
  | NTExpr (nt_expr, p) -> 
    let nt_expr' = List.map Utils.tr_fst nt_expr in
    let _ = check_d_ids_nt_expr nt_expr' in 
    NTExpr (nt_expr, p)
  | SynthAttr (nt, attr, p) -> 
    let _ = check_d_ids_nt_expr [Utils.tr_fst nt] in 
    let _ = check_d_ids_attribute attr in 
    SynthAttr (nt, attr, p)
  | OwnSynthAttr (attr, p) -> 
    let _ = check_d_ids_attribute attr in 
    OwnSynthAttr (attr, p)
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map call exprs, p) 
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | InhAttr _
  | PhConst _ 
  | StrConst _ -> expr
  | ActLit _ -> assert false

let rec check_nt_expr_refs: 
  prod_rule_map -> (Nt.t * int option * int option) list -> 
  Lexing.position -> (Nt.t * int option * int option) list 
= fun prm nt_expr p -> match nt_expr with 
| (nt1, idx1, idx2) :: (nt2, idx3, idx4) :: tl ->
  if (not (Nt.Set.mem nt2 (Nt.Map.find nt1 prm))) 
  then 
    let sub_expr_str = 
      Utils.capture_output Ast.pp_print_nt_with_dots [(nt1, idx1, idx2); (nt2, idx3, idx4)] in
    Utils.error ("Dot notation " ^ sub_expr_str ^ " is an invalid reference" ) p
  else (nt1, idx1, idx2) :: check_nt_expr_refs prm ((nt2, idx3, idx4) :: tl) p
| _ -> nt_expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_prod_rule_nt_exprs: prod_rule_map -> Nt.Set.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_prod_rule_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_expr, p) -> 
    if (not (Nt.Set.mem (List.hd nt_expr |> Utils.tr_fst) nts)) 
    then 
      Utils.error (Format.asprintf "Nonterminal %a not found in current production rule RHS or type annotation" Nt.pp (List.hd nt_expr |> Utils.tr_fst)) p
    else
      let nt_expr = check_nt_expr_refs prm nt_expr p in 
      NTExpr (nt_expr, p) 
  | SynthAttr ((nt, idx1, idx2), attr, p) -> 
    if (not (Nt.Set.mem nt nts)) 
    then 
      Utils.error (Format.asprintf "Nonterminal %a not found in current production rule RHS or type annotation" Nt.pp nt) p
    else SynthAttr ((nt, idx1, idx2), attr, p)
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map call exprs, p) 
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | InhAttr _
  | OwnSynthAttr _
  | StrConst _ -> expr
  | ActLit _ -> assert false

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_type_annot_nt_exprs: prod_rule_map -> Nt.Set.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_type_annot_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_expr, p) -> 
    if (not (Nt.Set.mem (List.hd nt_expr |> Utils.tr_fst) nts)) 
    then Utils.error (Format.asprintf "Nonterminal %a not found in current production rule RHS or type annotation" Nt.pp (List.hd nt_expr |> Utils.tr_fst)) p
    else
      let nt_expr = check_nt_expr_refs prm nt_expr p in 
      NTExpr (nt_expr, p) 
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map call exprs, p) 
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | SynthAttr _
  | InhAttr _
  | OwnSynthAttr _
  | StrConst _ -> expr
  | ActLit _ -> assert false


(* Every SMT constraint must contain some nonterminal. Everything else is trivial, ie, reduces to either 
   a constant True or False. This alone is not a problem, but these constraints mess up the check 
   of whether or not a constraint is applicable to a given derivation tree in dpll.ml *)
let check_for_nonterminals expr p = 
  let nts = Ast.get_nts_from_expr expr in 
  match nts with 
  | [] -> 
    let msg = "SMT constraint must include some nonterminal" in 
    Utils.error msg p 
  | _ -> expr

let rec check_for_ambiguous_derived_fields ast df expr rhs = 
  let r = check_for_ambiguous_derived_fields ast df in 
  match expr with 
  | NTExpr ((nt, idx, _) :: nts, p) -> 
    let rhs_nts = Ast.nts_of_rhs rhs in 
    let matching_rhs_nts = List.filter (fun nt' -> Nt.equal nt nt') rhs_nts in 
    if List.length matching_rhs_nts > 1 then 
     let msg = Format.asprintf "Derived field %a is defined ambiguously. More concretely, the definition of %a contains some nonterminal expression <nt_1>.<nt_2>...<nt_n> where some <nt_i> has multiple occurrences in its production rule (and hence the nonterminal expression could evaluate to more than one term, depending on which occurrence you pick)." Nt.pp df Nt.pp df in 
     Utils.error msg p
    else (
      let element = Ast.find_element ast nt in 
      match element with 
      | Ast.TypeAnnotation _ -> Ok ()
      | Ast.ProdRule (_, _, rhss, _) -> 
        if List.length rhss > 1 && idx <> None then (
          let msg = Format.asprintf "NTExpr within derived field %a must be defined in all possible RHSs" Nt.pp df in 
          Utils.error msg p
        ) else 
          Res.seq_ (List.map (r (NTExpr (nts, p))) rhss)
    )
  | NTExpr ([], _) -> Ok ()
  | BinOp (expr1, _, expr2, _) -> 
    let _ = r expr1 rhs in 
    r expr2 rhs
  | UnOp (_, expr, _) -> 
    r expr rhs
  | CompOp (expr1, _, expr2, _) -> 
    let _ = r expr1 rhs in 
    r expr2 rhs
  | BVCast (_, expr, _) 
  | Singleton (expr, _) -> r expr rhs
  | BuiltInFunc (_, exprs, _) ->
    Res.seq_ (List.map (fun e -> r e rhs) exprs)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _
  | SynthAttr _
  | InhAttr _
  | OwnSynthAttr _
  | EmptySet _  -> Ok ()
  | ActLit _ -> assert false

let check_syntax_prod_rule: ast -> prod_rule_map -> Nt.Set.t -> prod_rule_rhs -> prod_rule_rhs
= fun ast prm nt_set rhs -> match rhs with 
| Rhs (ges, scs, prob, p) ->
  let ges' = List.map Ast.nt_of_grammar_element ges in
  let scs = List.map (fun sc -> match sc with 
  | AttrDef (nt2, expr, p) -> 
    let expr = check_dangling_identifiers nt_set p expr in 
    let expr = check_prod_rule_nt_exprs prm (Nt.Set.of_list ges') expr in
    AttrDef (nt2, expr, p)
  | DerivedField (nt2, expr, p) -> (
    let _ = check_for_ambiguous_derived_fields ast nt2 expr rhs in 
    let expr = check_dangling_identifiers nt_set p expr in 
    if (not (Nt.Set.mem nt2 nt_set)) then Utils.error (Format.asprintf "Dangling identifier <%a>" Nt.pp nt2) p else
    if (not (List.mem nt2 ges')) then Utils.error 
      (Format.asprintf "DerivedField LHS identifier %a is not present on the RHS of the corresponding production rule" Nt.pp nt2) p else
    let expr = check_prod_rule_nt_exprs prm (Nt.Set.of_list ges') expr in
      DerivedField (nt2, expr, p)
    )
  | SmtConstraint (expr, p) -> 
    let expr = check_for_nonterminals expr p in
    let expr = check_dangling_identifiers nt_set p expr in 
    let expr = check_prod_rule_nt_exprs prm (Nt.Set.of_list ges') expr in
    SmtConstraint (expr, p)
  ) scs in 
  let _ = List.map (fun ge -> 
    if not (Nt.Set.mem ge nt_set) then Utils.error (Format.asprintf "Dangling identifier <%a>" Nt.pp ge) p 
    else ge
  ) ges' in
  Rhs (ges, scs, prob, p)
| StubbedRhs _ -> assert false

let rhss_contains_nt nt rhss = 
  List.exists (fun rhs -> match rhs with 
  | Rhs (ges, _, _, _) -> List.exists (fun ge -> match ge with 
    | Nonterminal (nt2, _, _, _, _) -> Nt.equal nt nt2
    | StubbedNonterminal stub -> Nt.equal nt stub.stands_for
  ) ges
  | StubbedRhs _ -> false
  ) rhss

let sort_ast: ast -> ast 
= fun ast -> 
  match TopologicalSort.canonicalize ast with 
  | Some ast -> ast
  (* In recursive grammars, ast does not need to be sorted, 
     as we cannot use the divide and conquer engines. *)
  | None -> ast

let check_if_recursive: ast -> bool 
= fun ast -> 
  match TopologicalSort.canonicalize ast with 
  | Some ast -> 
    List.exists (fun element -> match element with
    | ProdRule (nt, _, rhss, _) -> 
      rhss_contains_nt nt rhss 
    | _ -> false
    ) ast
  | None -> true

let check_vacuity: ast -> ast 
= fun ast -> 
  if ast = [] then Utils.error_no_pos "Grammar is empty after dead rule removal"
  else ast

let remove_circular_deps: ast -> ast 
= fun ast -> 
  List.map (fun element -> match element with
    | TypeAnnotation _ -> element 
    | ProdRule (nt, inhs, rhss, p) -> let rhss = List.map (fun rhs -> match rhs with
        | StubbedRhs _ -> rhs 
        | Rhs (nt, scs, prob, p) -> 
          let smt_exprs = List.filter (fun sc -> match sc with
          | SmtConstraint _ -> true 
          | AttrDef _ -> true (* for now, handle attributes at the SMT level *)
          | DerivedField _ -> false
          ) scs in 
          let dependencies = List.filter (fun sc -> match sc with
          | SmtConstraint _ -> false 
          | DerivedField _ -> true
          | AttrDef _ -> false
          ) scs in
          let dependencies = match TopologicalSort.canonicalize_scs dependencies with 
          | None -> dependencies
          | Some cycle ->
            let msg = Format.asprintf "Derived field cyclic dependency detected: %a\n" 
              (Lib.pp_print_list Nt.pp " ") cycle
            in
            Utils.error msg p
          in 
          Rhs (nt, smt_exprs @ dependencies, prob, p)
      ) rhss in 
      ProdRule (nt, inhs, rhss, p)
  ) ast

let check_scs_for_dep_terms: semantic_constraint list -> semantic_constraint list  
= fun scs -> 
  let dep_terms = List.fold_left (fun acc sc -> match sc with 
| DerivedField (nt, _, _) -> Nt.Set.add nt acc 
  | _ -> acc
  ) Nt.Set.empty scs in
  let deps_to_convert = List.fold_left (fun acc sc -> match sc with 
  | DerivedField _ -> acc
  | AttrDef (_, expr, _)
  | SmtConstraint (expr, _) -> 
    let nts = Ast.get_nts_from_expr expr |> Nt.Set.of_list in 
    let intersection = Nt.Set.inter dep_terms nts in
    if Nt.Set.is_empty intersection then acc
    else 
      let deps_to_convert = intersection in
      Nt.Set.union acc deps_to_convert
  ) Nt.Set.empty scs in 
  List.fold_left (fun acc sc -> match sc with 
  | DerivedField (nt, _, p) -> 
    if Nt.Set.mem nt deps_to_convert then (
      let msg = Format.asprintf "Derived field %a mentioned in semantic constraint"
        Nt.pp nt 
      in 
      Utils.error msg p
    ) else sc :: acc
  | AttrDef _ 
  | SmtConstraint _ -> sc :: acc
  ) [] scs |> List.rev

let check_smt_exprs_for_dep_terms: ast -> ast 
= fun ast -> 
  List.map (fun element -> match element with 
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = check_scs_for_dep_terms scs in 
    TypeAnnotation (nt, ty, scs, p)
  | ProdRule (nt, inhs, rhss, p) -> 
    let rhss = List.map (fun rhs -> match rhs with
    | Rhs (ges, scs, prob, p) -> Rhs (ges, check_scs_for_dep_terms scs, prob, p)
    | StubbedRhs _ -> rhs
    ) rhss in 
    ProdRule (nt, inhs, rhss, p)
  ) ast

(* The parser automatically parses all hardcoded string as string constants. 
   But, sometimes, they are actually placeholders, which are handled differently 
   by the type system. So, do the conversion here where necessary. *)
let str_const_to_ph_const ast = 
  let rec handle_expr = function 
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | StrConst (ph, p) -> PhConst (ph, p)
  | Singleton (expr, p) -> Singleton (handle_expr expr, p)
  | BVCast (len, expr, p) -> BVCast (len, handle_expr expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (handle_expr expr1, op, handle_expr expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, handle_expr expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (handle_expr expr1, op, handle_expr expr2, p) 
  | BuiltInFunc (func, exprs, p) -> BuiltInFunc (func, List.map handle_expr exprs, p) 
  | NTExpr _ 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | SynthAttr _
  | InhAttr _
  | OwnSynthAttr _
  | PhConst _ as expr -> expr
  | ActLit _ -> assert false
  in

  let handle_sc ty sc = match sc with 
  | SmtConstraint _ -> sc 
  | AttrDef _ -> sc
  | DerivedField (nt, expr, p) -> 
    let expr = 
      if ty = Placeholder then handle_expr expr else expr 
    in 
    DerivedField (nt, expr, p)
  in

  List.map (fun element -> match element with 
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (handle_sc ty) scs in 
    TypeAnnotation (nt, ty, scs, p)
  | ProdRule (nt, inhs, rhss, p) -> 
    ProdRule (nt, inhs, rhss, p)
  ) ast


let language_emptiness_check ast start_symbol = 
  let start_element = List.hd ast in 
  let ast = TopologicalSort.dead_rule_removal_2 ast start_symbol in 
  (* Dead rule removal may change order -- put start symbol back *)
  let ast = start_element :: List.filter (fun element -> match element with 
  | Ast.ProdRule (nt, _, _, _) 
  | Ast.TypeAnnotation (nt, _, _, _) -> nt <> start_symbol 
  ) ast 
  in
  let add_productive_nts ast productive_nts = 
    List.fold_left (fun acc element -> match element with 
    | TypeAnnotation (nt, _, _, _) -> Nt.Set.add nt acc 
    | ProdRule (nt, _, rhss, _) ->
      (* Does there exist some RHS for which all NTs are productive? *)
      if List.exists (fun rhs -> 
        let nts = Ast.nts_of_rhs rhs in 
        List.for_all (fun nt -> Nt.Set.mem nt acc) nts 
      ) rhss
      then
        Nt.Set.add nt acc 
      else acc
    ) productive_nts ast  
  in
  let productive_nt_set = Nt.Set.empty in 
  let productive_nt_set = Utils.recurse_until_fixpoint productive_nt_set Nt.Set.equal (add_productive_nts ast) in 
  if Nt.Set.equal productive_nt_set (Ast.nts_of_ast ast) then
  (*if Nt.Set.mem start_symbol productive_nt_set then   *)
    () 
  else 
    let unproductive = Nt.Set.diff (Ast.nts_of_ast ast) productive_nt_set |> Nt.Set.to_list in
    Utils.error_no_pos 
      (Format.asprintf "CFG has empty language. Unproductive nonterminals: %a (check for an infinite recursion in the grammar)" 
        (Lib.pp_print_list (fun ppf nt -> Format.fprintf ppf "<%a>" Nt.pp nt) ", ") unproductive)
      

let check_probabilities nt rhss p = 
  (* Omitting probabilities is legal (assumed uniform distribution) *)
  if List.for_all (fun rhs -> match rhs with 
  | StubbedRhs _ -> true 
  | Rhs (_, _, None, _) -> true 
  | Rhs (_, _, Some _, _) -> false 
  ) rhss then rhss 
  else 
    let rhss, total_probability = List.fold_left (fun (acc_rhss, acc_prob) rhs -> 
      match rhs with 
      | Rhs (_, _, Some prob, _) -> 
        acc_rhss @ [rhs], acc_prob +. prob
      | Rhs (_, _, None, p) -> 
        let msg = Format.asprintf "Production rule options for nonterminal <%a> must either all contain probability annotations, or none of them" Nt.pp nt in 
        Utils.error msg p
      | StubbedRhs _ -> assert false
    ) ([], 0.0) rhss in 
    let epsilon = 1e-12 in
    if abs_float (total_probability -. 1.0) > epsilon then
      let msg = Format.asprintf "Production rule probabilities for nonterminal <%a> must add to 1.0" Nt.pp nt in 
      Utils.error msg p
    else rhss

let check_no_redefinitions rhs = match rhs with 
  | StubbedRhs _ -> rhs 
  | Rhs (_, scs, _, p) -> 
    let derived_fields = List.concat_map (fun sc -> match sc with 
    | Ast.DerivedField (nt, _, _) -> [nt] 
    | SmtConstraint _ | AttrDef _ -> []
    ) scs in 
    let derived_fields' = derived_fields |> Nt.Set.of_list |> Nt.Set.to_list in 
    if not (List.equal Nt.equal derived_fields derived_fields') then 
      let msg = Format.asprintf "Production rule RHS %a contains more than one derived field definition for the same nonterminal (derived field (you might also have to look at the type annotations to find the duplicates)"
        Ast.pp_print_prod_rule_rhs rhs 
      in 
      Utils.error msg p
    else 
      rhs 

let check_syntax: prod_rule_map -> Nt.Set.t -> ast -> ast 
= fun prm nt_set ast -> 
  (*let ast = sort_ast ast in*) (* Maybe need this in non-dpll engines? *)
  let start_symbol = match ast with 
  | Ast.ProdRule (nt, _, _, _) :: _ 
  | Ast.TypeAnnotation (nt, _, _, _) :: _ -> nt
  | [] -> Utils.crash "empty grammar"
  in 
  let ast = str_const_to_ph_const ast in
  let ast = Utils.recurse_until_fixpoint ast (=) remove_circular_deps in
  let ast = Utils.recurse_until_fixpoint ast (=) check_smt_exprs_for_dep_terms in
  let ast = check_vacuity ast in
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, inhs, rhss, p) -> 
    let rhss = List.map (check_syntax_prod_rule ast prm nt_set) rhss in
    let rhss = List.map check_no_redefinitions rhss in
    let rhss = check_probabilities nt rhss p in 
    ProdRule (nt, inhs, rhss, p)
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (fun sc -> match sc with 
    | AttrDef (nt2, expr, p) ->
      let expr = check_dangling_identifiers nt_set p expr in 
      let expr = check_prod_rule_nt_exprs prm (Nt.Set.singleton nt) expr in
      AttrDef (nt2, expr, p)
    | DerivedField (nt2, expr, p) ->
      let expr = check_dangling_identifiers nt_set p expr in  
      if (not (Nt.Set.mem nt2 nt_set)) then Utils.error (Format.asprintf "Dangling identifier <%a>" Nt.pp nt2) p else
      if (not (nt2 = nt)) then Utils.error (Format.asprintf "DerivedField LHS identifier %a is not present in the corresponding type annotation" Nt.pp nt2) p else
      let expr = check_type_annot_nt_exprs prm (Nt.Set.singleton nt) expr in
      DerivedField (nt2, expr, p) 
    | SmtConstraint (expr, p) -> 
      let expr = check_for_nonterminals expr p in
      let expr = check_dangling_identifiers nt_set p expr in  
      let expr = check_type_annot_nt_exprs prm (Nt.Set.singleton nt) expr in
      SmtConstraint (expr, p)
    ) scs in 
    TypeAnnotation (nt, ty, scs, p)
  ) ast in 
  let _ = language_emptiness_check ast start_symbol in
  ast
