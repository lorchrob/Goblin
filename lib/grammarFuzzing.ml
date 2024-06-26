(* DANIYAL: File with basic mutation examples *)

open Ast
open SygusAst

(*  <S> ::= <A> <B> <C> 
    <A> ::= <B> <C>
    <C> ::= <D>
    ---> 
    <S> ::= <B> <A> <C>
    <A> ::= <C> <B>
    <C> ::= 
*)
(* Mutating the grammar, but without a concrete packet. *)
let mutate_grammar: ast -> ast 
= fun ast -> List.map (fun element -> match element with
  | ProdRule (nt, ge1 :: ge2 :: ges, scs) -> ProdRule (nt, ge2 :: ge1 :: ges, scs)
  | ProdRule (nt, _, _) -> ProdRule (nt, [], [])
  | TypeAnnotation _ -> element
  ) ast

let has_rgid_length_with_value_2 _ = assert false 

let has_rgid_list_with_length_n _ = assert false 

let mutate _ = assert false

(* Mutating a concrete packet *)
let rec mutate_concrete_packet: sygus_ast -> sygus_ast
= fun sygus_ast -> match sygus_ast with 
| Node (constructor, children) -> 
  if constructor = "REJECTED_GROUPS" && 
     has_rgid_length_with_value_2 children && 
     has_rgid_list_with_length_n children 
  then mutate sygus_ast  
  else Node (constructor, List.map mutate_concrete_packet children)
| _ -> sygus_ast