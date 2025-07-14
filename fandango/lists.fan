<start> ::= <list> 
<list> ::= <element> " :: " <list> | <element> ;
<element> ::= <ascii_lowercase_letter>+ ;

where exists <e1> in <element>: str(<e1>) == "foobar" ; 
