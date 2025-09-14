open Goblin 
open Ast

let input = Parsing.parse "
<C> ::= <B>
<S> ::= <A> <C> | <S> ;
<A> ::= <B> | <S> ;
<Z> ::= <L> <N> <Z> ;
<B> :: BitVec(16) ;
<L> :: BitVec(16) ;
<N> :: BitVec(16) ;

" in 
let x = canonicalize input in
pp_print_ast x ;
let y = dead_rule_removal input "S" in
pprint_ast y ;
let z = check_well_formed_rules input in
match z with
| true -> "well formed grammar"
| false -> "recursive loop"

let () = input
