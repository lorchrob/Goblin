module A = Ast

(* term is constructor + args *)
type clp_term = 
| Leaf of string 
| FunctionApp of string * clp_term list 
| List of clp_term list

type clp_ty = A.il_type 
type clp_sc = A.expr
type clp_element = 
| Term of clp_term 
| Destructor of clp_sc 
| SemanticConstraint of clp_sc

type clp_rule = 
  | ProdRule of clp_term * clp_element list
  | LeafRule of clp_term * string * clp_ty * clp_sc list
  | FieldExtractorDirect of clp_term
  | FieldExtractorIndirect of clp_term * clp_term list

type clp_program = clp_rule list

let rec pp_print_clp_term: Format.formatter -> clp_term -> unit
= fun ppf t -> match t with 
  | Leaf s -> Format.pp_print_string ppf (String.uppercase_ascii s)
  | FunctionApp (c, args) ->
    Format.fprintf ppf "%s(%a)"
      (String.lowercase_ascii c)
      (Lib.pp_print_list pp_print_clp_term ", ") args
  | List ts -> 
    Format.fprintf ppf "[%a]"
      (Lib.pp_print_list pp_print_clp_term ", ") ts

let expr_of_sc sc = match sc with 
| A.SyGuSExpr expr -> expr 
| Dependency (nt, expr) -> CompOp (NTExpr ([], [nt, None]), Eq, expr)

let pp_print_clp_element: Format.formatter -> clp_element -> unit 
= fun ppf element -> match element with 
| Term t -> pp_print_clp_term ppf t 
| Destructor sc 
| SemanticConstraint sc -> 
  (* NOTE: there is logic in pp_print_expr to print clp mode NTs correctly *)
  A.pp_print_expr ppf sc

let pp_print_clp_program: Format.formatter -> clp_program -> unit 
= fun ppf clp_program -> List.iter (fun rule -> match rule with 
| ProdRule (t, es) -> 
  Format.fprintf ppf "%% prod rule \n %a :- \n\t%a.\n\n"
    pp_print_clp_term t 
    (Lib.pp_print_list pp_print_clp_element ", ") es
| LeafRule (t, s, ty, []) -> 
  Format.fprintf ppf "%% leaf rule \n %a :- %s in %a.\n\n" 
    pp_print_clp_term t 
    (String.uppercase_ascii s)
    A.pp_print_ty ty 
| LeafRule (t, s, ty, scs) -> 
  let scs = List.map (fun sc -> SemanticConstraint sc) scs in
  Format.fprintf ppf "%% leaf rule \n %a :- %s in %a, %a.\n\n" 
    pp_print_clp_term t 
    (String.uppercase_ascii s)
    A.pp_print_ty ty 
    (Lib.pp_print_list pp_print_clp_element ", ") scs
| FieldExtractorIndirect (t, ts) -> 
  Format.fprintf ppf "%% field extractor \n %a :- \n\t%a.\n\n"
    pp_print_clp_term t 
    (Lib.pp_print_list pp_print_clp_term ", ") ts
| FieldExtractorDirect t -> 
  Format.fprintf ppf "%% field extractor \n %a.\n\n"
    pp_print_clp_term t 
) clp_program

let rec create_field_extractors: A.ast -> string list -> clp_rule list
= fun ast nt_expr -> match nt_expr with 
| [] 
| _ :: [] -> []
| nt1 :: nt2 :: [] -> 
  let extractor = nt1 ^ "_" ^ nt2 in 
  let nt1_rhss = List.find_map (function 
  | A.ProdRule (nt, rhss) -> if nt1 = nt then Some rhss else None
  | TypeAnnotation _ -> None
  ) ast |> Option.get in
  List.mapi (fun i rhs -> match rhs with 
    | A.StubbedRhs _ -> assert false 
    | A.Rhs (ges, _) ->
      let leaves = List.map (function 
      | A.StubbedNonterminal _ -> assert false
      | Nonterminal (nt, _) -> 
        if nt2 = nt then nt2 else "_"
      ) ges in
      let instances_of_nt2 = List.filter (fun leaf -> leaf <> "_") leaves in
      let instances_of_nt2 = List.mapi (fun i nt -> nt ^ (string_of_int i)) instances_of_nt2 in
      let _, leaves = List.fold_left (fun (acc_i, acc_leaves) leaf -> 
        if leaf = "_" then acc_i, acc_leaves @ [leaf]
        else 
          acc_i + 1, acc_leaves @ [leaf ^ (string_of_int acc_i)]
      ) (0, []) leaves in
      FieldExtractorDirect (FunctionApp (extractor, [FunctionApp (nt1 ^ (string_of_int i), List.map (fun l -> Leaf l) leaves); List (List.map (fun l -> Leaf l) instances_of_nt2)]))
  ) nt1_rhss

| nt1 :: nt2 :: rest -> 
  let leaf = Utils.last nt_expr ^ "s" in
  let nt1_extractor = nt1 ^ "_" ^ leaf in 
  let nt2_extractor = nt2 ^ "_" ^ leaf in 
  let nt1_rhss = List.find_map (function 
  | A.ProdRule (nt, rhss) -> if nt1 = nt then Some rhss else None
  | TypeAnnotation _ -> None
  ) ast |> Option.get in
  let field_extractors = List.mapi (fun i rhs -> match rhs with
    | A.StubbedRhs _ -> assert false 
    | A.Rhs (ges, _) -> 
      let instances_of_nt2 = List.filter_map (function 
      | A.StubbedNonterminal _ -> None
      | Nonterminal (nt, _) -> 
        if nt2 = nt then Some nt2 else None
      ) ges in
      if List.is_empty instances_of_nt2 then [] else
      let instances_of_nt2 = List.mapi (fun i nt -> nt ^ (string_of_int i)) instances_of_nt2 in
      let lhs_term = 
        FunctionApp (nt1_extractor, 
          [FunctionApp (nt1 ^ (string_of_int i), List.map (fun l -> Leaf l) instances_of_nt2); 
           Leaf leaf]) 
      in 
      let extract_terms = List.mapi (fun i s ->
        FunctionApp (nt2_extractor, [Leaf s; Leaf (leaf ^ string_of_int i)])
      ) instances_of_nt2 in
      let append_term = 
        FunctionApp ("append", (List.mapi (fun i _ -> Leaf (leaf ^ (string_of_int i))) instances_of_nt2) @ [Leaf leaf])
      in 
      [FieldExtractorIndirect (lhs_term, extract_terms @ [append_term])]
  ) nt1_rhss |> List.flatten in 
  field_extractors @ (create_field_extractors ast (nt2 :: rest))

let clp_program_of_ast: Ast.ast -> clp_program 
= fun ast -> List.fold_left (fun acc element -> match element with 
| A.ProdRule (nt, rhss) -> 
  (* Create a CLP rule for each prod rule RHS *)
  let rules = List.mapi (fun i rhs -> match rhs with 
  | A.StubbedRhs _ -> Utils.crash "unexpected case in clp_program_of_ast"
  | A.Rhs (_, scs) -> 
    let nts = A.nts_of_rhs rhs in
    let leaves = List.map (fun nt -> Leaf nt) nts in 
    let t = FunctionApp (nt, [FunctionApp (nt ^ (string_of_int i), leaves)]) in
    let nt_exprs = List.concat_map (fun sc -> A.get_nts_from_expr2 (expr_of_sc sc)) scs in
    let pairs = List.filter_map (fun nt_expr -> 
      if List.length nt_expr < 2 then None 
      else 
        Some (List.hd nt_expr |> fst, Utils.last nt_expr |> fst)  
    ) nt_exprs in 
    let destructors = List.map (fun (l, r) -> 
      let destructor_string = l ^ "_" ^ r ^ "s" in
      Term (FunctionApp (destructor_string, [Leaf l; Leaf (r ^ "s")]))  
    ) pairs in
    let scs = List.map (fun sc -> SemanticConstraint (expr_of_sc sc)) scs in
    let terms = List.map (fun nt -> Term (FunctionApp (nt, [Leaf nt]))) nts in
    let es = terms @ destructors @ scs in
    ProdRule (t, es)
  ) rhss in 
  (* Create field extractors for nt_exprs with dot notation *)
  let scs = A.scs_of_element element in
  let nt_exprs = List.concat_map (fun sc -> A.get_nts_from_expr2 (expr_of_sc sc)) scs in
  let nt_exprs = List.filter (fun nt_expr -> List.length nt_expr > 1) nt_exprs in
  let field_extractors = List.concat_map (create_field_extractors ast) (List.map (List.map fst) nt_exprs) in
  acc @ rules @ field_extractors
| TypeAnnotation (nt, ty, scs) -> 
  (* Create a CLP rule for each type annotation *)
  let scs = List.map expr_of_sc scs in
  acc @ [LeafRule (FunctionApp(nt, [Leaf nt]), nt, ty, scs)]
) [] ast