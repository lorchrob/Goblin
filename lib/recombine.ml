(* NOTE: This module's logic might fail if a subproblem has its own subproblems *)

let extract_stub str =
  let open Str in
  let re = regexp "^\\(_stub[0-9]+\\).*" in
  if string_match re str 0 then
    matched_group 1 str
  else
    ""

  let replace_stub: string -> SygusAst.sygus_ast list -> SygusAst.sygus_ast option
= fun possible_stub sygus_asts ->
  List.find_opt (fun sygus_ast -> match sygus_ast with 
  | SygusAst.IntLeaf _ | BVLeaf _ | BLLeaf _ | VarLeaf _ -> false 
  | Node (constructor, _) -> 
    (* To compare stubs, we only need the stub ID prefix "_stubN"*)
    (extract_stub possible_stub = extract_stub constructor) && 
    (extract_stub possible_stub <> "") 
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
 