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


(* Main function *)
let () = 
  Debug.parse_args ();

  (* ignore (Pipeline.main_pipeline 
  "
  <S> ::= <A> { <A> > 0; };
  <A> :: Int { <A> < 100; }; 
  "); *)

  (* let input = 
    "
    <SAE_PACKET> ::= <COMMIT> ;
    <COMMIT> ::= <AUTH_ALGO> <REJECTED_GROUPS> <AC_TOKEN_CONTAINER> 
       {<AUTH_ALGO> <- int_to_bitvector(16, 3);};

    <AUTH_ALGO> :: BitVector(16)
     { 
    <AUTH_ALGO> = int_to_bitvector(16, 0) lor 
    <AUTH_ALGO> = int_to_bitvector(16, 3); 
    };
 
       <STATUS_CODE> :: BitVector(16);

       <AUTH_SEQ_COMMIT> :: BitVector(16)   
    { <AUTH_SEQ_COMMIT> <- 0b0000000000000001; }; 
     <GROUP_ID> :: BitVector(16);
    
    <PASSWORD_IDENTIFIER> ::= <PASSWD_ELEMENT_ID> <PASSWD_ID_LENGTH> <PASSWD_ELEMENT_ID_EXTENSION> <PASSWD_ID>; 
    
    <PASSWD_ELEMENT_ID> :: BitVector(8) 
    { <PASSWD_ELEMENT_ID> <- int_to_bitvector(8, 255); };
    
    <PASSWD_ID_LENGTH> :: BitVector(8)
    { <PASSWD_ID_LENGTH> <- int_to_bitvector(8, 2); };
    
    <PASSWD_ELEMENT_ID_EXTENSION> :: BitVector(8)
    { <PASSWD_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 33); };
    
    
    <PASSWD_ID> :: BitVector(8);
    
    <REJECTED_GROUPS> ::= <RG_ELEMENT_ID> <RG_ID_LENGTH> <RG_ELEMENT_ID_EXTENSION> <RG_ID_LIST>
    { <RG_ID_LENGTH> <- int_to_bitvector(8, length(<RG_ID_LIST>)); };
    
    <RG_ELEMENT_ID> :: BitVector(8) 
    { <RG_ELEMENT_ID> <- int_to_bitvector(8, 255); };
    
    <RG_ID_LENGTH>   :: BitVector(8); 
    
    <RG_ELEMENT_ID_EXTENSION> :: BitVector(8) 
    { <RG_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 92); };
    
    <RG_ID_LIST> ::= <RG_ID> | <RG_ID> <RG_ID_LIST>;
     
    <RG_ID> :: BitVector(8);
    
    <AC_TOKEN_CONTAINER> ::= <AC_ELEMENT_ID> <AC_ID_LENGTH> <AC_ELEMENT_ID_EXTENSION> 
                         <AC_TOKEN_ELEMENT>
    { <AC_ID_LENGTH> <- int_to_bitvector(8, length(<AC_TOKEN_ELEMENT>)); };
    
    <AC_ELEMENT_ID> :: BitVector(8) 
    { <AC_ELEMENT_ID> <- int_to_bitvector(8, 255); };
    
    <AC_ID_LENGTH> :: BitVector(8);
    
    <AC_ELEMENT_ID_EXTENSION> :: BitVector(8)
    { <AC_ELEMENT_ID_EXTENSION> <- int_to_bitvector(8, 93); };
    
    <AC_TOKEN_ELEMENT> :: BitList;
    
    
    "
   in
   let _ = Pipeline.main_pipeline input in
   ()

   *)


   
   (* <GROUP_ID> = int_to_bitvector(16, 15) => 
   length(<AC_TOKEN>) <= 2048;
    length(<SCALAR>) = 384 land length(<ELEMENT>) = 384;
    <GROUP_ID> = int_to_bitvector(16, 16) => 
    length(<SCALAR>) = 512 land length(<ELEMENT>) = 512;
    <GROUP_ID> = int_to_bitvector(16, 17) => 
    length(<SCALAR>) = 768 land length(<ELEMENT>) = 768;
    <GROUP_ID> = int_to_bitvector(16, 18) => 
    length(<SCALAR>) = 1024 land length(<ELEMENT>) = 1024;
    <GROUP_ID> = int_to_bitvector(16, 19) => 
    length(<SCALAR>) = 256 land length(<ELEMENT>) = 512;
    <GROUP_ID> = int_to_bitvector(16, 20) => 
    length(<SCALAR>) = 48 land length(<ELEMENT>) = 96;
    <GROUP_ID> = int_to_bitvector(16, 21) => 
    length(<SCALAR>) = 64 land length(<ELEMENT>) = 128; *)

  let commit_grammar = Utils.parse (GrammarFuzzing.read_grammar "bin/commit.txt") in
  let confirm_grammar = Utils.parse (GrammarFuzzing.read_grammar "bin/confirm.txt") in
  let commit_confirm_grammar = Utils.parse (GrammarFuzzing.read_grammar "bin/commit-confirm.txt") in
  GrammarFuzzing.runFuzzer [commit_grammar; confirm_grammar; commit_confirm_grammar;]
    
  (* let commit_confirm_grammar = Utils.parse (GrammarFuzzing.read_grammar "/home/pirwani/Desktop/WiFiPacketGen/bin/commit-confirm.txt") in
  let pkt = Mutationops. in
  match pkt with
  | Some x -> Ast.pp_print_ast Format.std_formatter x ;
  | None -> print_endline "fail" *)
