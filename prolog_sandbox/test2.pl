:- use_module(library(clpfd)).

% S -> A B { A.C > B }
s(s1(A, B)) :-
    a(A),
    b(B), 
    A = a(C, _),
    C #> B.

% S -> C { C > 3 }
s(s2(C)) :-
    c(C),
    C #> 3.

% A -> C D
a(a(C, D)) :-
    c(C),
    d(D).

% Leaf rules
b(B) :- B in 0..100.
c(C) :- C in 0..100.
d(D) :- D in 0..100.

