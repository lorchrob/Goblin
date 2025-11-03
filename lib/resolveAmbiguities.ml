(* Note that if this ambiguity resolution strategy becomes too laborious, 
   there is a simpler (but less efficient) approach of also introducing new 
   symbols at the grammar level 
   (the current strategy avoids this). *)

module A = Ast
module TC = TypeChecker
type nt = (string * int option) list

let cartesian_product lst1 lst2 =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) lst2) lst1

(* Given an input expr, convert it into a (generated) conjunction of 
   all the possible expressions the base expr could represent. 
   E.g., <A>.<B>.<C> > 0 could produce 
   <A>.<B0>.<C0> > 1 and <A>.<B0>.<C1> > 1 and <A>.<B1>.<C0> > 1 and <A>.<B1>.<C1> > 1,
   assuming there are ambiguous references for <A>.<B> and <B>.<C>.   *)
let rec generate_all_possible_exprs: TC.context -> string list -> A.expr -> A.expr list
(* nts represents the list of nonterminals that could be referenced in the expression *)
= fun ctx nts expr -> 
  let r = generate_all_possible_exprs ctx nts in
  match expr with 
  | NTExpr ([], nt_expr, p) -> 
    let rec helper nts nt_expr = match nt_expr with 
      | [] -> Utils.crash "Impossible case in generate_all_possible_exprs"
      (* If there is already an index, we eliminate the ambiguity. 
         This might allow us to include indices in the input syntax for no (or little) extra cost *)
      | (nt', Some idx) :: [] -> [[(nt', Some idx)]]
      | (nt', None) :: [] -> 
        let nt_options = 
          nts |>
          List.filter (fun nt'' -> String.equal nt' nt'') |>
          List.mapi (fun i rhs_nt -> (rhs_nt, Some i)) 
        in 
        (* let nt_options  *)
        List.map (fun nt_option -> [nt_option]) nt_options
      | (nt', Some idx) :: nt_expr' -> 
        let nt_options = [(nt', Some idx)] in 
        let nts' = match Utils.StringMap.find nt' ctx with 
        | ADT options -> List.flatten options
        | _ -> [nt']
        in
        let recursive_options = helper nts' nt_expr' in
        let all_combos = cartesian_product nt_options recursive_options in 
        List.map (fun (nt, nt_expr) -> nt :: nt_expr) all_combos
      | (nt', None) :: nt_expr' -> 
        let nt_options = 
          nts |>
          List.filter (fun nt'' -> String.equal nt' nt'') |>
          List.mapi (fun i rhs_nt -> (rhs_nt, Some i)) 
        in 
        let nts' = match Utils.StringMap.find nt' ctx with 
        | ADT options -> List.flatten options
        | _ -> [nt']
        in
        let recursive_options = helper nts' nt_expr' in
        let all_combos = cartesian_product nt_options recursive_options in 
        List.map (fun (nt, nt_expr) -> nt :: nt_expr) all_combos
    in 
    let exprs = helper nts nt_expr in 
    List.map (fun e -> A.NTExpr ([], e, p)) exprs
  | NTExpr _ ->  Utils.crash "Impossible case in generate_all_possible_exprs: encountered NTExpr with context, but dot notation should not be desugared yet"
  | A.Match _ -> Utils.crash "Impossible case in generate_all_possible_exprs: encountered Match, but dot notation should not be desugared yet"
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
  | SynthAttr _ -> assert false

let process_sc: TC.context -> string list -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx nts sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    let exprs = generate_all_possible_exprs ctx nts expr in
    let expr = match exprs with 
      | _ :: _ :: _ -> Utils.error ("Dependent term '" ^ nt ^ "' is defined ambiguously") p
      | expr :: _ -> expr
      | [] -> Utils.crash "unexpected case"
    in
    DerivedField (nt, expr, p)
  | AttrDef _ -> assert false
  | SmtConstraint (expr, p) -> 
    let exprs = generate_all_possible_exprs ctx nts expr in
    let expr = List.fold_left (fun acc expr -> A.BinOp (expr, GLAnd, acc, p)) (BConst (true, p)) exprs in
    SmtConstraint (expr, p)

let process_sc_to_list: TC.context -> string list -> A.semantic_constraint -> A.semantic_constraint list
= fun ctx nts sc -> match sc with 
  | A.DerivedField (nt, expr, p) -> 
    let exprs = generate_all_possible_exprs ctx nts expr in 
    let _ = match exprs with 
      (*!! TODO: Reimplement this check so it doesn't falsely flag bug5.gbl *)
      (*| _ :: _ :: _ -> Utils.error ("Dependent term '" ^ nt ^ "' is defined ambiguously") p*)
      | expr :: _ -> expr
      | [] -> Utils.crash "unexpected case"
    in
    List.map (fun expr -> A.DerivedField (nt, expr, p)) exprs
  | SmtConstraint (expr, p) -> 
    let exprs = generate_all_possible_exprs ctx nts expr in
    List.map (fun expr -> A.SmtConstraint (expr, p)) exprs
  | AttrDef _ -> assert false

(* Same as resolve_ambiguities, but we desugar to a list of 
   semantic constraints rather than a conjunction of generated 
   constraints. The conjunction makes more sense in the sygus encoding
   (where we can encode the big conjunction with pattern matching 
   over all the cases), 
   while the list is required for the DPLL encoding where we explore one 
   case at a time, ignoring constraints that aren't applicable in that case. *)
let resolve_ambiguities_dpll: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, rhss, p) ->
  (*!! Need to assume universally unique IDs *)
  let nts = List.concat_map A.nts_of_rhs rhss in 
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs, prob, _) -> 
    let scs = List.concat_map (process_sc_to_list ctx nts) scs in
    (* Filter out scs with dot notation expressions of the form <nt1>[n], where 
       [n] does not apply to this production rule *)
    let scs = List.filter (function 
    | A.AttrDef _ -> assert false
    | A.SmtConstraint (expr, _)  
    | A.DerivedField (_, expr, _) ->
      let nts = A.get_nts_from_expr2 expr |> List.map List.hd in
      List.for_all (fun (nt1, idx1) -> 
        List.exists (function 
        | A.StubbedNonterminal _ -> false 
        | A.Nonterminal (nt2, idx2, _) -> nt1 = nt2 && idx1 = idx2 
        ) ges
      ) nts  
    ) scs in 
    A.Rhs (ges, scs, prob, p) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, rhss, p)
| TypeAnnotation (nt, ty, scs, p) -> 
  let scs = List.concat_map (process_sc_to_list ctx [nt]) scs in
  TypeAnnotation (nt, ty, scs, p)
) ast 

let resolve_ambiguities: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, rhss, p) ->
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs, prob, _) -> A.Rhs (ges, List.map (process_sc ctx (A.nts_of_rhs rhs)) scs, prob, p) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, rhss, p)
| TypeAnnotation (nt, ty, scs, p) -> TypeAnnotation (nt, ty, List.map (process_sc ctx [nt]) scs, p)
) ast  
