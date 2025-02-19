module A = Ast
module SM = Utils.StringMap



let create_substitutions (dups : int list SM.t) : (string * int) list list =
  (* Generate all combinations of (key, value) pairs from the map *)
  let choices = SM.bindings dups |> List.map (fun (key, values) -> List.map (fun v -> (key, v)) values) in
  
  (* Compute the Cartesian product of these choices to get all possible substitutions *)
  let rec cartesian_product = function
    | [] -> [[]]
    | hd :: tl ->
      let rest = cartesian_product tl in
      List.concat (List.map (fun h -> List.map (fun r -> h :: r) rest) hd)
  in
  
  cartesian_product choices
    

let rec process_expr: A.expr -> A.expr 
= fun expr -> 
  let r = process_expr in
  match expr with 
  | A.Match (nt_ctx, nt, cases) -> 
    let cases = List.map (fun case -> match case with 
    | A.CaseStub _ -> case 
    | Case (nts, expr) -> 
      (* Map of nt -> # of occurrences for nts *)
      let dups = List.fold_left (fun acc (_, (nt, _)) ->
        match SM.find_opt nt acc with 
        | None -> SM.add nt 1 acc
        | Some i -> SM.add nt (i+1) acc
      ) SM.empty nts in 
      let dups = SM.filter (fun _ i -> 
        i > 1
      ) dups in
      (* Rename duplicated nts *)
      let nts, _ = List.fold_left (fun (acc_nts, acc_dups) (nt_ctx, (nt, idx)) -> 
        match SM.find_opt nt acc_dups with 
        | None -> (nt_ctx, (nt, idx)) :: acc_nts, acc_dups
        | Some i -> 
          let acc_nts = (nt_ctx, (nt, Some i)) :: acc_nts in
          let acc_dups = SM.add nt (i-1) acc_dups in
          acc_nts, acc_dups  
      ) ([], dups) nts in 
      let nts = List.rev nts in
      (* Generate new expr for every possible combination of ambiguous references *)
      let expr_nts = A.get_nts_from_expr_shallow expr in 
      let duplicated_expr_nts = List.filter (fun nt -> SM.mem nt dups) expr_nts in
      let dups = SM.filter (fun nt _ -> List.mem nt duplicated_expr_nts) dups in
      let dups = SM.fold (fun nt i acc -> 
        let generated_nts = 
          Utils.replicate nt i |> 
          List.mapi (fun i _ -> i+1)
        in
        SM.add nt generated_nts acc  
      ) dups SM.empty in
      let substitutions = create_substitutions dups in
      let exprs = List.map (A.rename expr) substitutions in
      (* Cover all possible cases with conjunction *)
      let expr = match exprs with 
      | init :: exprs -> List.fold_left (fun acc expr -> A.BinOp (expr, LAnd, acc)  
      ) init exprs 
      | [] -> expr 
      in
      A.Case (nts, expr)
    ) cases in 
    Match (nt_ctx, nt, cases)
  | BinOp (expr1, op, expr2) -> BinOp (r expr1, op, r expr2)
  | UnOp (op, expr) -> UnOp (op, r expr)
  | CompOp (expr1, op, expr2) -> CompOp (r expr1, op, r expr2)
  | Length expr -> Length (r expr)
  | BVConst _ 
  | BLConst _ 
  | BConst _ 
  | BVCast _  
  | StrConst _
  | IntConst _
  | NTExpr _ -> expr

let process_sc: A.semantic_constraint -> A.semantic_constraint 
= fun sc -> match sc with 
  | A.Dependency (nt, expr) -> Dependency (nt, process_expr expr)
  | SyGuSExpr expr -> SyGuSExpr (process_expr expr)

let resolve_ambiguities: A.ast -> A.ast 
= fun ast -> List.map (fun element -> match element with
| A.ProdRule (nt, rhss) -> 
  let rhss = List.map (fun rhs -> match rhs with 
  | A.Rhs (ges, scs) -> A.Rhs (ges, List.map process_sc scs) 
  | StubbedRhs _ -> rhs 
  ) rhss in 
  A.ProdRule (nt, rhss)
| TypeAnnotation (nt, ty, scs) -> TypeAnnotation (nt, ty, List.map process_sc scs)
) ast 