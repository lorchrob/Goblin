module A = Ast
module TC = TypeChecker

let cartesian_product lst1 lst2 =
  List.map (fun x -> List.map (fun y -> (x, y)) lst2) lst1 |> List.flatten

(*!! TODO *)
(* Errors out of there is some NTExpr with an empty list of NTs. This 
   occurs if the user NTExpr was ill-formed, leading to a vacuous implicit universal quantification. *)
let check_for_empty_nt_exprs expr = expr

(*!! TODO *)
(* Returns true iff `expr` contains some pair of NTExprs making the constraint unfalsifiable 
   (ie, <nt>@0 = <nt>@1). If so, we can filter out the constraint. *)
let impossible_nt_expr _expr = false

let gen_idx_options_from_head ges nt idx1 idx2 = 
  (* Collect all the nonterminals in this RHS that could apply *)
  List.filter (function 
  | A.StubbedNonterminal _ -> false
  | A.Nonterminal (id, Some idx', Some idx'', _, _) -> (
    match idx1, idx2 with 
    | None, None -> id = nt 
    | Some idx1, None -> id = nt && idx1 = idx' 
    | None, Some idx2 -> id = nt && idx2 = idx''
    | Some idx1, Some idx2 -> id = nt && idx1 = idx' && idx2 = idx''
  )
  | A.Nonterminal _ -> assert false
  ) ges 
  |> 
  (* Pick out the indices *)
  List.map (function 
  | A.StubbedNonterminal _ -> assert false 
  | A.Nonterminal (_, Some idx', Some idx'', _, _) -> idx', idx'' 
  | A.Nonterminal _ -> assert false) 
  |> 
  List.split

(* Every dot notation reference, if potentially ambiguous, should include an index (in bounds) 
   to disambiguate *)
let rec gen_all_exprs 
= fun ctx ast ges expr -> 
  let r = gen_all_exprs ctx ast ges in
    (*Format.printf "input ges: %a\n"
      (Lib.pp_print_list A.pp_print_grammar_element ", ") ges;*)
  match expr with 
  | A.NTExpr ((nt, idx1, idx2) :: (nt2, idx3, idx4) :: nts, p) -> 
    (* Find all possible references for idx1 and idx2 in this RHS *)
    let idx1, idx2 = gen_idx_options_from_head ges nt idx1 idx2 in
    let element = A.find_element ast nt in 
    (* Find which RHSs could be referenced by (nt2, idx3, idx4) *)
    let new_gess = match element with  
    | ProdRule (_, _, rhss, _) -> 
      let rhss = List.filteri (fun i rhs -> 
        (idx3 = None || idx3 = Some i) && 
        (idx4 = None || 
          List.exists (fun ge -> 
            match ge with | A.Nonterminal (_, _, Some idx4', _, _) -> idx4 = Some idx4' | _ -> false
          ) (A.ges_of_rhs rhs))
      ) rhss in 
      List.map A.ges_of_rhs rhss
    | TypeAnnotation (nt, _, _, p) -> 
      if (idx3 = None || idx3 = Some 0) || 
         (idx4 = None || idx4 = Some 0) 
      then [[A.Nonterminal (nt, Some 0, Some 0, [], p)]]
      else 
        let msg = Format.asprintf "Nonterminal reference %s@%d is not valid" 
          nt 
          (Option.get idx3)
        in 
        Utils.error msg p
    in
    (* Collect all possible tails *)
    let ntss = 
      List.map (fun new_ges -> gen_all_exprs ctx ast new_ges (NTExpr ((nt2, idx3, idx4) :: nts, p))) new_gess 
      |> List.flatten 
    in 
    let ntss = List.map (function
    | A.NTExpr (nts, _) -> 
      (* We recursively generate fully disambiguated (indexed) NTExprs, so `Option.get` 
         should not fail *)
      List.map (fun (nt, idx1, idx2) -> nt, Option.get idx1, Option.get idx2) nts 
    | _ -> assert false 
    ) ntss
    in
    let head_options = List.map2 (fun idx1 idx2 -> nt, idx1, idx2) idx1 idx2 in
    (*Format.printf "head_options 1: %a\n"
      (Lib.pp_print_list (fun _ppf (nt, idx1, idx2) -> Format.printf "(%s, %d, %d)" nt idx1 idx2) ", ") head_options;
    Format.printf "ges: %a\n"
      (Lib.pp_print_list A.pp_print_grammar_element ", ") ges;*)
    (* Combine every possible head with every possible tail *)
    let nt_exprs = cartesian_product head_options ntss in
    List.map (fun (nt_head, nt_tl) -> 
      (* Re-wrap indices with Some to satisfy OCaml's type checker *)
      A.NTExpr (List.map (fun (nt, idx1, idx2) -> nt, Some idx1, Some idx2) (nt_head :: nt_tl), p)
    ) nt_exprs 
  | A.NTExpr ((nt, idx1, idx2) :: [], p) -> 
    let idx1, idx2 = gen_idx_options_from_head ges nt idx1 idx2 in
    (*let head_options = List.map2 (fun idx1 idx2 -> nt, idx1, idx2) idx1 idx2 in 
    Format.printf "head_options 2: %a\n"
      (Lib.pp_print_list (fun _ppf (nt, idx1, idx2) -> Format.printf "(%s, %d, %d)" nt idx1 idx2) ", ") head_options;
    Format.printf "ges: %a\n"
      (Lib.pp_print_list A.pp_print_grammar_element ", ") ges;*)
    List.map2 (fun idx1 idx2 -> A.NTExpr ((nt, Some idx1, Some idx2) :: [], p)) idx1 idx2
  | A.NTExpr _ -> assert false
  | BinOp (expr1, op, expr2, p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.BinOp (e1, op, e2, p)) pairs
  | UnOp (op, expr, p) ->
    let exprs = r expr in 
    List.map (fun e -> A.UnOp (op, e, p)) exprs
  | CompOp (expr1, op, expr2, p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.CompOp (e1, op, e2, p)) pairs 
  | BuiltInFunc (func, [expr], p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.BuiltInFunc (func, [e], p)) exprs 
  | BuiltInFunc (func, [expr1; expr2], p) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.BuiltInFunc (func, [e1; e2], p)) pairs 
  | Singleton (expr, p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.Singleton (e, p)) exprs
  | BVCast (i, expr, p) -> 
    let exprs = r expr in 
    List.map (fun e -> A.BVCast (i, e, p)) exprs 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _ 
  | EmptySet _ -> [expr]
  | BuiltInFunc _ ->
    let msg = Format.asprintf "Bad function arity in expression %a" 
      A.pp_print_expr expr in 
    Utils.error msg (A.pos_of_expr expr)
  | InhAttr _
  | SynthAttr _ -> assert false

let process_sc
= fun ctx ast ges sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    let exprs = gen_all_exprs ctx ast ges expr in (
    match exprs with 
    | [] ->
      let msg = Format.asprintf "Derived field %s contains some nonterminal reference that could not be evaluated. For example, if nonterminal <nt> has only one production rule, then `<nt>@1` cannot be evaluated (use <nt>@0 instead)." nt in 
      Utils.error msg p
    | [expr] -> 
      let expr = check_for_empty_nt_exprs expr in 
      [A.DerivedField (nt, expr, p)]
    | _ -> 
      let msg = Format.asprintf "Derived field %s is defined ambiguously. More concretely, the definition of %s contains some nonterminal expression <nt_1>.<nt_2>...<nt_n> where some <nt_i> has multiple occurrences in its production rule (and hence the nonterminal expression could evaluate to more than one term, depending on which occurrence you pick)." 
      nt nt in 
      Utils.error msg p
    )
  | SmtConstraint (expr, p) ->
    let exprs = gen_all_exprs ctx ast ges expr in 
    if List.length exprs = 0 then 
      let msg = Format.asprintf "Semantic constraint contains some nonterminal reference that could not be evaluated. For example, if nonterminal <nt> has only one production rule, then `<nt>@1` cannot be evaluated (use <nt>@0 instead)." in 
      Utils.error msg p
    else 
      let exprs = List.map check_for_empty_nt_exprs exprs in 
      let exprs = List.filter (fun expr -> not (impossible_nt_expr expr)) exprs in
      List.map (fun expr -> A.SmtConstraint (expr, p)) exprs
  | AttrDef _ -> assert false

let resolve_ambiguities: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, ias, rhss, p) ->
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs, prob, _) -> 
    let scs = List.map (process_sc ctx ast ges) scs in
    A.Rhs (ges, List.flatten scs, prob, p) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, ias, rhss, p)
| TypeAnnotation _ -> element (* no constraints left here (inlined) *) 
) ast 
