:- use_module(library(clpfd)).

% S -> A B { A.C > B }
s(s1(A, B)) :-
    a(A),
    b(B),
    a_c(A, C),
    C #> B.

% A -> C D | C
a(a1(C, D)) :- 
    c(C), 
    d(D).
a(a2(C))    :- 
    c(C).

% Leaf rules
b(B) :- B in 0..100.
c(C) :- C in 0..100.
d(D) :- D in 0..100.

% Field extractor for A.C
a_c(a1(C, _), C).
a_c(a2(C),   C).
