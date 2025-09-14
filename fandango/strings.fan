<start> ::= <i1> " , " <i2> ;
<i1> ::= <ascii_lowercase_letter>+ ;
<i2> ::= <ascii_lowercase_letter>+ ;

# where <i1> == <i2> ;
where str(<i1>) == str(<i2>) + "foo" ;


