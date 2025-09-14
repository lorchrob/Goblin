let replace_stub: string -> SygusAst.sygus_ast list -> SygusAst.sygus_ast option
= fun possible_stub sygus_asts ->
  List.find_opt (fun sygus_ast -> match sygus_ast with 
  | SygusAst.IntLeaf _ | BVLeaf _ | BLLeaf _ | VarLeaf _ 
  | BoolLeaf _ | StrLeaf _ | SetLeaf _ | UnitLeaf -> false 
  | Node ((constructor, _), _) -> 
    (* To compare stubs, we only need the stub ID prefix "_stubN"*)
    Utils.extract_base_name possible_stub = Utils.extract_base_name constructor
    || 
    (* For type annotations, we don't use a stub ID *)
    possible_stub = constructor
  ) sygus_asts

(* Invariant: First element of sygus_asts is the combined AST *)
let rec recombine: SygusAst.sygus_ast list -> SygusAst.sygus_ast 
= fun sygus_asts -> 
  match sygus_asts with 
| [] -> assert false
| SetLeaf _ :: _ | IntLeaf _ :: _ | BVLeaf _ :: _ 
| BLLeaf _ :: _ | BoolLeaf _ :: _ | StrLeaf _ :: _ | UnitLeaf :: _ -> List.hd sygus_asts
| Node (constructor, children) :: sygus_asts ->
  let children = List.map (fun sygus_ast -> recombine (sygus_ast :: sygus_asts)) children in
  Node (constructor, children)

(* Might need to recurse on what is currently a "_" in this pattern match *)
| VarLeaf possible_stub :: _ -> 
  match replace_stub possible_stub sygus_asts with 
  | Some sygus_ast -> recombine (sygus_ast :: sygus_asts)
  | None -> List.hd sygus_asts
 
