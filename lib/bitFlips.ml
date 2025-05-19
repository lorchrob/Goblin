open SygusAst

let random_bit_flip b = 
  if Random.bool () then not b else b

let flip_bit_list lst = 
  if Random.bool () then List.map random_bit_flip lst else lst

let rec flip_bits: sygus_ast -> sygus_ast 
= fun sygus_ast -> Random.self_init (); match sygus_ast with 
| IntLeaf _ | BVLeaf _ | VarLeaf _ | BoolLeaf _ | StrLeaf _ -> sygus_ast 
| BLLeaf bits -> 
  BLLeaf (flip_bit_list bits)
| Node (constructor, children) -> 
  let children = List.map flip_bits children in 
  Node (constructor, children)