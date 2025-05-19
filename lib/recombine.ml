let extract_stub str =
  let str = String.lowercase_ascii str in
  let open Str in
  (* Remove optional trailing "_con" or "_con123" *)
  let str =
    global_replace (regexp "_con[0-9]*$") "" str
  in
  (* Remove prefix "_stub" *)
  try
    let re = regexp "_stub\\([0-9]+\\)" in
    let _ = search_forward re str 0 in
    matched_group 0 str
  with Not_found ->
    ""

let replace_stub: string -> SygusAst.sygus_ast list -> SygusAst.sygus_ast option
= fun possible_stub sygus_asts ->
  List.find_opt (fun sygus_ast -> match sygus_ast with 
  | SygusAst.IntLeaf _ | BVLeaf _ | BLLeaf _ | VarLeaf _ | BoolLeaf _ | StrLeaf _ -> false 
  | Node (constructor, _) -> 
    (* To compare stubs, we only need the stub ID prefix "_stubN"*)
    ((extract_stub possible_stub = extract_stub constructor) && 
     (extract_stub possible_stub <> ""))
    || 
    (* For type annotations, we don't use a stub ID *)
    (possible_stub = constructor)
  ) sygus_asts

(* Invariant: First element of sygus_asts is the combined AST *)
let rec recombine: SygusAst.sygus_ast list -> SygusAst.sygus_ast 
= fun sygus_asts -> match sygus_asts with 
| [] -> assert false
| IntLeaf _ :: _ | BVLeaf _ :: _ | BLLeaf _ :: _ | BoolLeaf _ :: _ | StrLeaf _ :: _ -> List.hd sygus_asts
| Node (constructor, children) :: sygus_asts ->
  let children = List.map (fun sygus_ast -> recombine (sygus_ast :: sygus_asts)) children in
  Node (constructor, children)

(* Might need to recurse on what is currently a "_" in this pattern match *)
| VarLeaf possible_stub :: _ -> 
  match replace_stub possible_stub sygus_asts with 
  | Some sygus_ast -> recombine (sygus_ast :: sygus_asts)
  | None -> List.hd sygus_asts
 