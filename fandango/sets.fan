<start> ::= <i1> ;
<i1> ::= <digit>+ | "-" <digit>+;

def member_of_set(num): 
  num in {1, 2, 3} 

# set membership constraint -- non-monotonic or not?
where member_of_set(int(<i1>)) ; 
