open SolverAst

let random_bit_flip b = 
  if Random.bool () then not b else b
 
let flip_bit_list lst = 
  if Random.bool () then List.map random_bit_flip lst else lst

let rec flip_bits: solver_ast -> solver_ast 
= fun solver_ast -> Random.self_init (); match solver_ast with 
| Leaf (BitList bits) -> 
  Leaf (BitList (flip_bit_list bits))
| Leaf _ | StubLeaf _ | Model _ | Infeasible -> solver_ast 
| Node (constructor, children) -> 
  let children = List.map flip_bits children in 
  Node (constructor, children)
