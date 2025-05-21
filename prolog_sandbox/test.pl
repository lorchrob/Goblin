:- use_module(library(clpfd)).

% S -> A B { A.C > B }
s(s(A, B)) :-
    a(A),
    b(B),
    A = a(C, _),
    C #> B.

% A -> C D
a(a(C, D)) :-
    c(C),
    d(D).

% Leaf rules
b(B) :- B in 0..100.
c(C) :- C in 0..100.
d(D) :- D in 0..100.
