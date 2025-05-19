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
  | NTExpr ([], nt_expr) -> 
    let rec helper nts nt_expr = match nt_expr with 
      | [] -> Utils.crash "Impossible case in generate_all_possible_exprs"
      (* If there is already an index, we eliminate the ambiguity. 
         This might allow us to include indices in the input syntax for no (or little) extra cost *)
      | (nt', Some idx) :: [] -> [[(nt', Some idx)]]
      | (nt', None) :: [] -> 
        (* let rhs_nts = match Utils.StringMap.find nt ctx with 
        | ADT options -> List.flatten options
        | _ -> [nt']
        in *)
        let nt_options = 
          nts |>
          List.filter (fun nt'' -> String.equal nt' nt'') |>
          List.mapi (fun i rhs_nt -> (rhs_nt, Some i)) 
        in 
        let nt_options = 
          if List.length nt_options = 1 
          then [(fst (List.hd nt_options), None)] 
          else nt_options 
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
        let nt_options = 
          if List.length nt_options = 1 
          then [(fst (List.hd nt_options), None)] 
          else nt_options 
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
    List.map (fun e -> A.NTExpr ([], e)) exprs
  | NTExpr _ ->  Utils.crash "Impossible case in generate_all_possible_exprs: encountered NTExpr with context, but dot notation should not be desugared yet"
  | A.Match _ -> Utils.crash "Impossible case in generate_all_possible_exprs: encountered Match, but dot notation should not be desugared yet"
  | BinOp (expr1, op, expr2) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.BinOp (e1, op, e2)) pairs
  | UnOp (op, expr) ->
    let exprs = r expr in 
    List.map (fun e -> A.UnOp (op, e)) exprs
  | CompOp (expr1, op, expr2) -> 
    let exprs1 = r expr1 in 
    let exprs2 = r expr2 in 
    let pairs = cartesian_product exprs1 exprs2 in
    List.map (fun (e1, e2) -> A.CompOp (e1, op, e2)) pairs
  | StrLength expr -> 
    let exprs = r expr in 
    List.map (fun e -> A.StrLength (e)) exprs
  | Length expr -> 
    let exprs = r expr in 
    List.map (fun e -> A.Length (e)) exprs
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | PhConst _
  | IntConst _ 
  | StrConst _ -> [expr]

let process_sc: TC.context -> string list -> A.semantic_constraint -> A.semantic_constraint 
= fun ctx nts sc -> match sc with 
  | A.Dependency (nt, expr) -> 
    let exprs = generate_all_possible_exprs ctx nts expr in
    let expr = match exprs with 
      | init :: exprs -> List.fold_left (fun acc expr -> A.BinOp (expr, GLAnd, acc)) init exprs 
      | [] -> expr 
    in
    Dependency (nt, expr)
  | SyGuSExpr expr -> 
    let exprs = generate_all_possible_exprs ctx nts expr in
    let expr = match exprs with 
      | init :: exprs -> List.fold_left (fun acc expr -> A.BinOp (expr, GLAnd, acc)) init exprs 
      | [] -> expr 
    in
    SyGuSExpr expr

let resolve_ambiguities: TC.context -> A.ast -> A.ast 
= fun ctx ast -> List.map (fun element -> match element with
| A.ProdRule (nt, rhss) -> 
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs) -> A.Rhs (ges, List.map (process_sc ctx (A.nts_of_rhs rhs)) scs) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, rhss)
| TypeAnnotation (nt, ty, scs) -> TypeAnnotation (nt, ty, List.map (process_sc ctx [nt]) scs)
) ast  