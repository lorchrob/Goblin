open Sbf

(* NOTES: 

  * Should use proper debugging output to not hamper performance, and so specific debug print statements
    don't get lost in the junk

  * I think some of the length constraints are written informally in comments rather than in the grammar
  * Test length constraint with RG_ID_LIST more rigorously
    * It should work, but it is conservatively rejected by the type checker. The type checker 
      should be more permissive in dependency expressions because we can concatenate bitlists and bitvectors and such.

  * The PASSWORD_IDENTIFIER nonterminal is optional in the notes, and it is a composite nonterminal 
    (not just a BitList). So, we should really model this with two 
    production rules-- one where it is present, and one where it is absent, and update the semantic constraints accordingly.
  * Same for REJECTED_GROUPS.
  
  * Using multiple versions of cvc5
*)

(* 
  TODO:
  1. Proper debug logging
  2. Support placeholder values (maybe string type)
*)

(* 
  <S> ::= <A> <E> { <E>.<B> < 0; };  --> 
  <S> ::= <A> <E> { 
    match <E> with 
    | <B> <C> -> <B> < 0; 
    | other case w/out <B> -> true;
  }; 
*)

(* Main function *)
let () = 
  Debug.parse_args ();

  let out = (Pipeline.main_pipeline 
    "
    <S> ::= <A> { <A>.<B>.<D> > <A>.<C>.<D>; };
    <A> ::= <B> <B> <C> <C>;
    <B> ::= <D>;
    <C> ::= <D>;
    <D> :: Int;
  ") in

  (* let out = (Pipeline.main_pipeline 
  "
    <S> ::= <A> { <A>.<B> + 3 < 0; <A>.<D> + 3 < 0; }; 
    <A> ::= <B> <C> | <D>;
    <B> :: Int;
    <C> :: Int;
    <D> :: Int;
    ") in *)


  print_endline out;