module SA = SygusAst
module A = Ast
module R = Res 

let (let*) = Res.(>>=)

(* Check
  1. The sygus_ast is an instance of the grammar (syntactic well-formedness)
  2. Sygus_ast respects every semantic constraint in ast (semantic well-formedness)

*)

let rec check_sygus_ast: Ast.ast -> SygusAst.sygus_ast -> (unit, string) result 
= fun ast sygus_ast -> match sygus_ast with 
  | Node (constructor, children) -> 
    let* _ = R.seq (List.map (check_sygus_ast ast) children) in 
    let rhss = List.find_map (fun element -> match element with
    | A.TypeAnnotation _ -> None 
    | A.ProdRule (nt, rhss) -> 
      if constructor = nt 
      then Some rhss
      else None
    ) ast in 
    if rhss = None then Error "Dangling constructor identifier" else 
    let rhss = Option.get rhss in 
    (* Find the matching production rule from ast, if one exists *)
    let rhs = List.find_opt (fun rhs -> match rhs with 
    | A.StubbedRhs _ -> false 
    | A.Rhs (ges, _) -> 
      if List.length ges != List.length children then false 
      else 
        List.for_all2 (fun child ge -> 
          match child, ge with 
          | _, A.StubbedNonterminal _ -> false 
          | SA.Node (constructor, _), Nonterminal nt -> constructor = nt
          | _, _ -> true
        ) children ges
    ) rhss in 
    if rhs = None then Error ("Could not find an associated production rule for constructor '" ^ constructor ^"'") else 
    let _rhs = Option.get rhs in 
    Ok ()
  | _ -> Ok ()