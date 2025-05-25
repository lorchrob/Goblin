module SA = SygusAst
module A = Ast
module R = Res 

let (let*) = Res.(>>=)

(* Check
  1. The sygus_ast is an instance of the grammar (syntactic well-formedness)
    a. The grammar starts at the start symbol 
    b. Each non-leaf node in the sygus ast, along with its children, 
       has some corresponding grammar production rule 
  2. Sygus_ast respects every semantic constraint in ast (semantic well-formedness)
*)

(* Hacky helper function because in the sygus implementation, we use the generated 
   constructor names in the sygus ast. *)

let check_start_symbol: Ast.ast -> SygusAst.sygus_ast -> (unit, string) result 
= fun ast sygus_ast -> match ast, sygus_ast with 
| A.ProdRule (nt, _) :: _, SA.Node (constructor, _) -> 
  if Utils.str_eq_ci nt (Utils.extract_base_name constructor) 
    then Ok () 
  else 
  Error ("Sygus AST root constructor '" ^ constructor ^ "' does not match the AST start symbol '" ^ nt ^ "'")
| A.TypeAnnotation _ :: _, _ -> Utils.crash "Unexpected case in check_start_symbol"
| _ -> Error "Sygus AST root node is a leaf node"

let rec check_syntax_semantics: Ast.ast -> SygusAst.sygus_ast -> (unit, string) result 
= fun ast sygus_ast -> match sygus_ast with 
  | Node (constructor, children) -> 
    (* In dpll divide and conquer module, 
       we get an extra nesting of stub and concrete NTs 
       for some reason. *)
    let skip_condition = 
      match children with 
      | [Node (constructor2, _)] ->
        Utils.str_eq_ci constructor (Utils.extract_base_name constructor2)
      | _ -> false
    in
    if skip_condition then check_syntax_semantics ast (List.hd children) else
      
    let* _ = R.seq (List.map (check_syntax_semantics ast) children) in
    (* Find this node's corresponding AST element *) 
    let nt_rhss = List.find_map (fun element -> match element with
    | A.TypeAnnotation (nt, _, _) -> 
      if Utils.str_eq_ci (Utils.extract_base_name constructor) nt 
      then Some (nt, []) 
      else None
    | A.ProdRule (nt, rhss) -> 
      if Utils.str_eq_ci (Utils.extract_base_name constructor) nt 
      then Some (nt, rhss)
      else None
    ) ast in 
    if nt_rhss = None then Error ("Dangling constructor identifier " ^ (Utils.extract_base_name constructor)) else 
    let nt, rhss = Option.get nt_rhss in 
    (* Find the matching production rule from ast, if one exists *)
    let rhs = List.find_opt (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> false 
    | A.Rhs (ges, _) -> 
      if List.length ges != List.length children then false 
      else 
        List.for_all2 (fun child ge ->  
          match child, ge with 
          | _, A.StubbedNonterminal _ -> false 
          | SA.Node (constructor, _), Nonterminal (nt, _) -> Utils.str_eq_ci (Utils.extract_base_name constructor) nt
          | _, _ -> true
        ) children ges
    ) rhss in 
    if rhss != [] && rhs = None then Error ("Could not find an associated production rule for constructor '" ^ constructor ^"'") else 
    let scs = match rhs with 
    | Some (StubbedRhs _) -> assert false 
    | Some (Rhs (_, scs)) -> scs 
    | None -> [] (* TODO: Need to analyze scs in type annotation case *)
    in 
    (* Evaluate each semantic constraint with concrete values from the sygus AST, and check 
       if all are satisfied *)
    let scs = List.map (fun sc -> match sc with 
    | A.SyGuSExpr expr -> 
      ComputeDeps.evaluate sygus_ast ast (ProdRule (nt, rhss)) expr
    | Dependency (nt, expr) -> 
      ComputeDeps.evaluate sygus_ast ast (ProdRule (nt, rhss)) (A.CompOp (NTExpr ([], [nt, None]), Eq, expr))
    ) scs in
    let b = List.exists (fun sc -> match sc with 
    | [A.BConst false] -> true 
    | [BConst true] -> false 
    | _ -> failwith "Unexpected pattern in check_syntax_semantics"
    ) scs in
    if b then Error ("Semantic constraint on constructor '" ^ constructor ^ " is falsified") else
    Ok ()
  | _ -> Ok ()

let check_sygus_ast: Ast.ast -> SygusAst.sygus_ast -> (unit, string) result 
= fun ast sygus_ast -> 
  let* _ = check_start_symbol ast sygus_ast in 
  check_syntax_semantics ast sygus_ast