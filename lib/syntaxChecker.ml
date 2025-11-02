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

  (*!! TODO: 
       Revisit dangling identifier checks (esp., for nonterminals on RHS not in constraints 

  *)
*)

open Ast
open Res 

module StringSet = Set.Make(
  struct type t = string 
  let compare = Stdlib.compare 
  end
)

type prod_rule_map = (Utils.StringSet.t) Utils.StringMap.t

(* Build production rule map, which is a map from each grammar nonterminal 
   to the list of valid nonterminal references *)
let build_prm: ast -> prod_rule_map
= fun ast -> 
  let prm = List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, rhss, p) -> 
    List.fold_left (fun acc rhss -> match rhss with 
    | Rhs (ges, _, _, _) -> 
      let grammar_elements = List.map Ast.grammar_element_to_string ges in
      let grammar_elements = Utils.StringSet.of_list grammar_elements in (
      match Utils.StringMap.find_opt nt acc with 
      | Some mem -> 
        if (Utils.StringSet.is_empty mem) then 
          Utils.error ("Nonterminal " ^ nt ^ " has both a type annotation and a production rule") p
        else Utils.StringMap.add nt (Utils.StringSet.union mem grammar_elements) acc 
      | None -> 
        Utils.StringMap.add nt grammar_elements acc
      )
    | StubbedRhs _ -> acc
    ) acc rhss
  | TypeAnnotation (nt, _, _, p) -> 
    match Utils.StringMap.find_opt nt acc with 
    | Some _ -> 
      Utils.error ("Nonterminal " ^ nt ^ " either has two type annotations, or has both a type annotation and a production rule") p
    | None -> 
      Utils.StringMap.add nt Utils.StringSet.empty acc 
  ) Utils.StringMap.empty ast in 
  prm

(* Build nonterminal set, which the set of nonterminals with either
   their own production rules or type annotations *)
let build_nt_set: ast -> Utils.StringSet.t 
= fun ast -> 
  List.fold_left (fun acc element -> match element with 
  | ProdRule (nt, _, _)
  | TypeAnnotation (nt, _, _, _) -> Utils.StringSet.add nt acc
  ) Utils.StringSet.empty ast

let rec check_dangling_identifiers: Utils.StringSet.t -> Lexing.position -> expr -> expr 
= fun nt_set p expr -> 
  let call = check_dangling_identifiers nt_set p in 
  let check_d_ids_nt_expr nt_expr = 
    List.iter (fun nt -> match Utils.StringSet.find_opt nt nt_set with 
    | None -> Utils.error ("Dangling identifier <" ^ nt ^ ">") p
    | Some _ -> ()
    ) nt_expr
  in
  match expr with 
  | NTExpr (nt_ctx, nt_expr, p) -> 
    let nt_expr' = List.map fst nt_expr in
    let _ = check_d_ids_nt_expr nt_expr' in 
    NTExpr (nt_ctx, nt_expr, p)
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | StrLength (expr, p) -> StrLength (call expr, p)
  | Length (expr, p) -> Length (call expr, p) 
  | SeqLength (expr, p) -> SeqLength (call expr, p) 
  | Match _ -> assert false (* -> Match (check_d_ids_nt_expr nt_expr, cases) *)
  | StrInRe (expr1, expr2, p) -> StrInRe (call expr1, call expr2, p) 
  | ReStar (expr, p) -> ReStar (call expr, p)
  | StrToRe (expr, p) -> StrToRe (call expr, p) 
  | ReConcat (exprs, p) -> ReConcat (List.map call exprs, p)
  | ReUnion (exprs, p) -> ReUnion (List.map call exprs, p) 
  | ReRange (expr1, expr2, p) -> ReRange (call expr1, call expr2, p)
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | UbvToInt (expr, p) -> UbvToInt (call expr, p)
  | SbvToInt (expr, p) -> SbvToInt (call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

let rec check_nt_expr_refs: prod_rule_map -> (string * int option) list -> Lexing.position -> (string * int option) list 
= fun prm nt_expr p -> match nt_expr with 
| (nt1, idx1) :: (nt2, idx2) :: tl ->
  if (not (Utils.StringSet.mem nt2 (Utils.StringMap.find nt1 prm))) 
  then 
    let sub_expr_str = Utils.capture_output Ast.pp_print_nt_with_dots [(nt1, idx1); (nt2, idx2)] in
    Utils.error ("Dot notation " ^ sub_expr_str ^ " is an invalid reference" ) p
  else (nt1, idx1) :: check_nt_expr_refs prm ((nt2, idx2) :: tl) p
| _ -> nt_expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_prod_rule_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_prod_rule_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_context, nt_expr, p) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr |> fst) nts)) 
    then 
      Utils.error ("Nonterminal " ^  (List.hd nt_expr |> fst) ^ " not found in current production rule RHS or type annotation") p
    else
      let nt_expr = check_nt_expr_refs prm nt_expr p in 
      NTExpr (nt_context, nt_expr, p) 
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | StrLength (expr, p) -> StrLength (call expr, p)
  | ReStar (expr, p) -> ReStar (call expr, p)
  | Length (expr, p) -> Length (call expr, p) 
  | SeqLength (expr, p) -> SeqLength (call expr, p) 
  | StrInRe (expr1, expr2, p) -> StrInRe (call expr1, call expr2, p) 
  | StrToRe (expr, p) -> StrToRe (call expr, p) 
  | ReConcat (exprs, p) -> ReConcat (List.map call exprs, p)
  | ReUnion (exprs, p) -> ReUnion (List.map call exprs, p) 
  | ReRange (expr1, expr2, p) -> ReRange (call expr1, call expr2, p)
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | UbvToInt (expr, p) -> UbvToInt (call expr, p)
  | SbvToInt (expr, p) -> SbvToInt (call expr, p)
  | Match _ -> assert false (* -> Match (check_nt_expr_refs prm nt_expr, cases) *)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

(* Check each nonterminal expression begins with a valid nonterminal
   and contains valid dot notation references *)
let rec check_type_annot_nt_exprs: prod_rule_map -> Utils.StringSet.t -> expr -> expr 
= fun prm nts expr -> 
  let call = check_type_annot_nt_exprs prm nts in
  match expr with 
  | NTExpr (nt_context, nt_expr, p) -> 
    if (not (Utils.StringSet.mem (List.hd nt_expr |> fst) nts)) 
    then Utils.error ("Nonterminal " ^  (List.hd nt_expr |> fst) ^ " not found in current production rule RHS or type annotation") p
    else
      let nt_expr = check_nt_expr_refs prm nt_expr p in 
      NTExpr (nt_context, nt_expr, p) 
  | EmptySet (ty, p) -> EmptySet (ty, p)
  | Singleton (expr, p) -> Singleton (call expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (call expr1, op, call expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, call expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (call expr1, op, call expr2, p) 
  | StrLength (expr, p) -> StrLength (call expr, p)
  | Length (expr, p) -> Length (call expr, p) 
  | SeqLength (expr, p) -> SeqLength (call expr, p) 
  | Match _ -> assert false(* -> Match (check_nt_expr_refs prm nt_expr, cases) *)
  | StrInRe (expr1, expr2, p) -> StrInRe (call expr1, call expr2, p) 
  | ReStar (expr, p) -> ReStar (call expr, p)
  | StrToRe (expr, p) -> StrToRe (call expr, p) 
  | ReConcat (exprs, p) -> ReConcat (List.map call exprs, p)
  | ReUnion (exprs, p) -> ReUnion (List.map call exprs, p) 
  | ReRange (expr1, expr2, p) -> ReRange (call expr1, call expr2, p)
  | BVCast (width, expr, p) -> BVCast (width, call expr, p)
  | UbvToInt (expr, p) -> UbvToInt (call expr, p)
  | SbvToInt (expr, p) -> SbvToInt (call expr, p)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ 
  | StrConst _ -> expr

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

let rec check_for_ambiguous_derived_fields ast expr rhs = 
  let r = check_for_ambiguous_derived_fields ast in 
  match expr with 
  | NTExpr (_, (nt, _) :: nts, p) -> 
    let rhs_nts = Ast.nts_of_rhs rhs in 
    let matching_rhs_nts = List.filter (fun nt' -> String.equal nt nt') rhs_nts in 
    if List.length matching_rhs_nts > 1 then 
      Error () 
    else (
      let element = Ast.find_element ast nt in 
      match element with 
      | Ast.TypeAnnotation _ -> Ok ()
      | Ast.ProdRule (_, rhss, _) -> 
        Res.seq_ (List.map (r (NTExpr ([], nts, p))) rhss)
    )
  | NTExpr (_, [], _) -> Ok ()
  | Match _ -> assert false 
  | BinOp (expr1, _, expr2, _) -> 
    let* _ = r expr1 rhs in 
    r expr2 rhs
  | UnOp (_, expr, _) -> 
    r expr rhs
  | StrInRe (expr1, expr2, _) 
  | ReRange (expr1, expr2, _)
  | CompOp (expr1, _, expr2, _) -> 
    let* _ = r expr1 rhs in 
    r expr2 rhs
  | StrLength (expr, _)
  | SeqLength (expr, _)
  | StrToRe (expr, _) 
  | ReStar (expr, _) 
  | Length (expr, _)  
  | BVCast (_, expr, _) 
  | UbvToInt (expr, _) 
  | SbvToInt (expr, _) 
  | Singleton (expr, _) -> r expr rhs
  | ReConcat (exprs, _) 
  | ReUnion (exprs, _) ->
    Res.seq_ (List.map (fun e -> r e rhs) exprs)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | PhConst _
  | IntConst _ 
  | StrConst _
  | EmptySet _  -> Ok ()

let check_syntax_prod_rule: ast -> prod_rule_map -> Utils.StringSet.t -> prod_rule_rhs -> prod_rule_rhs
= fun ast prm nt_set rhs -> match rhs with 
| Rhs (ges, scs, prob, p) ->
  let ges' = List.map Ast.grammar_element_to_string ges in
  let scs = List.map (fun sc -> match sc with 
  | DerivedField (nt2, expr, p) -> (
    let expr = check_dangling_identifiers nt_set p expr in 
    match check_for_ambiguous_derived_fields ast expr rhs with 
    | Error () -> 
      let msg = Format.asprintf "Derived field %s is defined ambiguously. More concretely, the definition of %s contains some nonterminal expression <nt_1>.<nt_2>...<nt_n> where some <nt_i> has multiple occurrences in its production rule (and hence the nonterminal expression could evaluate to more than one term, depending on which occurrence you pick)." 
      nt2 nt2 in 
      Utils.error msg p
    | Ok () -> 
      if (not (Utils.StringSet.mem nt2 nt_set)) then Utils.error ("Dangling identifier <" ^ nt2 ^ ">") p else
      if (not (List.mem nt2 ges')) then Utils.error ("DerivedField LHS identifier " ^ nt2 ^ " is not present on the RHS of the corresponding production rule") p else
      let expr = check_prod_rule_nt_exprs prm (Utils.StringSet.of_list ges') expr in
      DerivedField (nt2, expr, p)
    )
  | SmtConstraint (expr, p) -> 
    let expr = check_for_nonterminals expr p in
    let expr = check_dangling_identifiers nt_set p expr in 
    let expr = check_prod_rule_nt_exprs prm (Utils.StringSet.of_list ges') expr in
    SmtConstraint (expr, p)
  ) scs in 
  let _ = List.map (fun ge -> 
    if not (Utils.StringSet.mem ge nt_set) then Utils.error ("Dangling identifier <" ^ ge ^ ">") p 
    else ge
  ) ges' in
  Rhs (ges, scs, prob, p)
| StubbedRhs _ -> assert false

let rhss_contains_nt nt rhss = 
  List.exists (fun rhs -> match rhs with 
  | Rhs (ges, _, _, _) -> List.exists (fun ge -> match ge with 
    | Nonterminal (nt2, _, _)
    | StubbedNonterminal (nt2, _) -> 
      nt = nt2
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
    | ProdRule (nt, rhss, _) -> 
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
    | ProdRule (nt, rhss, p) -> let rhss = List.map (fun rhs -> match rhs with
        | StubbedRhs _ -> rhs 
        | Rhs (nt, scs, prob, p) -> 
          let sygus_exprs = List.filter (fun sc -> match sc with
          | SmtConstraint _ -> true 
          | DerivedField _ -> false
          ) scs in 
          let dependencies = List.filter (fun sc -> match sc with
          | SmtConstraint _ -> false 
          | DerivedField _ -> true
          ) scs in
          let dependencies = match TopologicalSort.canonicalize_scs dependencies with 
          | None -> dependencies
          | Some cycle ->
            let msg = Format.asprintf "Derived field cyclic dependency detected: %a\n" 
              (Lib.pp_print_list Format.pp_print_string " ") cycle
            in
            Utils.error msg p
          in 
          Rhs (nt, sygus_exprs @ dependencies, prob, p)
      ) rhss in 
      ProdRule (nt, rhss, p)
  ) ast

let check_scs_for_dep_terms: semantic_constraint list -> semantic_constraint list  
= fun scs -> 
  let dep_terms = List.fold_left (fun acc sc -> match sc with 
| DerivedField (nt, _, _) -> StringSet.add nt acc 
  | _ -> acc
  ) StringSet.empty scs in
  let deps_to_convert = List.fold_left (fun acc sc -> match sc with 
  | DerivedField _ -> acc
  | SmtConstraint (expr, _) -> 
    let nts = Ast.get_nts_from_expr expr |> StringSet.of_list in 
    let intersection = StringSet.inter dep_terms nts in
    if StringSet.is_empty intersection then acc
    else 
      let deps_to_convert = intersection in
      StringSet.union acc deps_to_convert
  ) StringSet.empty scs in 
  List.fold_left (fun acc sc -> match sc with 
  | DerivedField (nt, _, p) -> 
    if StringSet.mem nt deps_to_convert then (
      let msg = Format.asprintf "Derived field %s mentioned in semantic constraint"
        nt 
      in 
      Utils.error msg p
    ) else sc :: acc
  | SmtConstraint _ -> sc :: acc
  ) [] scs |> List.rev

let check_sygus_exprs_for_dep_terms: ast -> ast 
= fun ast -> 
  List.map (fun element -> match element with 
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = check_scs_for_dep_terms scs in 
    TypeAnnotation (nt, ty, scs, p)
  | ProdRule (nt, rhss, p) -> 
    let rhss = List.map (fun rhs -> match rhs with
    | Rhs (ges, scs, prob, p) -> Rhs (ges, check_scs_for_dep_terms scs, prob, p)
    | StubbedRhs _ -> rhs
    ) rhss in 
    ProdRule (nt, rhss, p)
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
  | UbvToInt (expr, p) -> UbvToInt (handle_expr expr, p)
  | SbvToInt (expr, p) -> SbvToInt (handle_expr expr, p)
  | BinOp (expr1, op, expr2, p) -> BinOp (handle_expr expr1, op, handle_expr expr2, p) 
  | UnOp (op, expr, p) -> UnOp (op, handle_expr expr, p) 
  | CompOp (expr1, op, expr2, p) -> CompOp (handle_expr expr1, op, handle_expr expr2, p) 
  | StrLength (expr, p) -> StrLength (handle_expr expr, p) 
  | SeqLength (expr, p) -> SeqLength (handle_expr expr, p) 
  | Length (expr, p) -> Length (handle_expr expr, p) 
  | Match (nt_ctx, nt, cases, p) -> 
    let cases = List.map (fun case -> match case with 
    | CaseStub _ -> case 
    | Case (nts, e) -> Case (nts, handle_expr e)
    ) cases in
    Match (nt_ctx, nt, cases, p) 
  | StrInRe (expr1, expr2, p) -> StrInRe (handle_expr expr1, handle_expr expr2, p) 
  | ReStar (expr, p) -> ReStar (handle_expr expr, p)
  | StrToRe (expr, p) -> StrToRe (handle_expr expr, p) 
  | ReConcat (exprs, p) -> ReConcat (List.map handle_expr exprs, p) 
  | ReUnion (exprs, p) -> ReUnion (List.map handle_expr exprs, p) 
  | ReRange (expr1, expr2, p) -> ReRange (handle_expr expr1, handle_expr expr2, p)
  | NTExpr _ 
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | IntConst _ 
  | PhConst _ as expr -> expr
  in

  let handle_sc ty sc = match sc with 
  | SmtConstraint _ -> sc 
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
  | ProdRule (nt, rhss, p) -> 
    ProdRule (nt, rhss, p)
  ) ast


let language_emptiness_check ast start_symbol = 
  let start_element = List.hd ast in 
  let ast = TopologicalSort.dead_rule_removal_2 ast start_symbol in 
  (* Dead rule removal may change order -- put start symbol back *)
  let ast = start_element :: List.filter (fun element -> match element with 
  | Ast.ProdRule (nt, _, _) 
  | Ast.TypeAnnotation (nt, _, _, _) -> nt <> start_symbol 
  ) ast 
  in
  let add_productive_nts ast productive_nts = 
    List.fold_left (fun acc element -> match element with 
    | TypeAnnotation (nt, _, _, _) -> Utils.StringSet.add nt acc 
    | ProdRule (nt, rhss, _) ->
      (* Does there exist some RHS for which all NTs are productive? *)
      if List.exists (fun rhs -> 
        let nts = Ast.nts_of_rhs rhs in 
        List.for_all (fun nt -> Utils.StringSet.mem nt acc) nts 
      ) rhss
      then
        Utils.StringSet.add nt acc 
      else acc
    ) productive_nts ast  
  in
  let productive_nt_set = Utils.StringSet.empty in 
  let productive_nt_set = Utils.recurse_until_fixpoint productive_nt_set Utils.StringSet.equal (add_productive_nts ast) in 
  if Utils.StringSet.equal productive_nt_set (Ast.nts_of_ast ast) then
  (*if Utils.StringSet.mem start_symbol productive_nt_set then   *)
    () 
  else 
    Utils.error_no_pos "CFG has empty language"

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
        let msg = Format.asprintf "Production rule options for nonterminal <%s> must either all contain probability annotations, or none of them" nt in 
        Utils.error msg p
      | StubbedRhs _ -> assert false
    ) ([], 0.0) rhss in 
    let epsilon = 1e-12 in
    if abs_float (total_probability -. 1.0) > epsilon then
      let msg = Format.asprintf "Production rule probabilities for nonterminal <%s> must add to 1.0" nt in 
      Utils.error msg p
    else rhss

let check_syntax: prod_rule_map -> Utils.StringSet.t -> ast -> ast 
= fun prm nt_set ast -> 
  (*let ast = sort_ast ast in*) (*!! Maybe need this in non-dpll engines? *)
  let start_symbol = match ast with 
  | Ast.ProdRule (nt, _, _) :: _ 
  | Ast.TypeAnnotation (nt, _, _, _) :: _ -> nt
  | [] -> Utils.crash "empty grammar"
  in 
  let ast = str_const_to_ph_const ast in
  let ast = Utils.recurse_until_fixpoint ast (=) remove_circular_deps in
  let ast = Utils.recurse_until_fixpoint ast (=) check_sygus_exprs_for_dep_terms in
  let ast = check_vacuity ast in
  let ast = List.map (fun element -> match element with 
  | ProdRule (nt, rhss, p) -> 
    let rhss = List.map (check_syntax_prod_rule ast prm nt_set) rhss in
    let rhss = check_probabilities nt rhss p in 
    ProdRule (nt, rhss, p)
  | TypeAnnotation (nt, ty, scs, p) -> 
    let scs = List.map (fun sc -> match sc with 
    | DerivedField (nt2, expr, p) ->
      let expr = check_dangling_identifiers nt_set p expr in  
      if (not (Utils.StringSet.mem nt2 nt_set)) then Utils.error ("Dangling identifier <" ^ nt2 ^ ">") p else
      if (not (nt2 = nt)) then Utils.error ("DerivedField LHS identifier " ^ nt2 ^ " is not present in the corresponding type annotation") p else
      let expr = check_type_annot_nt_exprs prm (Utils.StringSet.singleton nt) expr in
      DerivedField (nt2, expr, p) 
    | SmtConstraint (expr, p) -> 
      let expr = check_for_nonterminals expr p in
      let expr = check_dangling_identifiers nt_set p expr in  
      let expr = check_type_annot_nt_exprs prm (Utils.StringSet.singleton nt) expr in
      SmtConstraint (expr, p)
    ) scs in 
    TypeAnnotation (nt, ty, scs, p)
  ) ast in 
  let _ = language_emptiness_check ast start_symbol in
  ast
