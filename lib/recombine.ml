(* NOTE: This module's logic might fail if a subproblem has its own subproblems *)

let replace_stub: string -> SygusAst.sygus_ast list -> SygusAst.sygus_ast option
= fun possible_stub sygus_asts ->
  List.find_opt (fun sygus_ast -> match sygus_ast with 
  | SygusAst.IntLeaf _ | BVLeaf _ | BLLeaf _ | VarLeaf _ -> false 
  | Node (constructor, _) -> possible_stub = constructor
  ) sygus_asts

let rec recombine: SygusAst.sygus_ast list -> SygusAst.sygus_ast 
= fun sygus_asts -> match sygus_asts with 
| [] -> assert false
| IntLeaf _ :: _ | BVLeaf _ :: _ | BLLeaf _ :: _ -> List.hd sygus_asts
| Node (constructor, children) :: sygus_asts ->
  let children = List.map (fun sygus_ast -> recombine (sygus_ast :: sygus_asts)) children in
  Node (constructor, children)

(* To deal with the above note, might need to recurse on what is currently a "_" in this pattern match *)
| VarLeaf possible_stub :: _ -> 
  match replace_stub possible_stub sygus_asts with 
  | Some sygus_ast -> sygus_ast 
  | None -> List.hd sygus_asts
 