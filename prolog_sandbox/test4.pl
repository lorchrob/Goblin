:- use_module(library(clpfd)).

% S -> A B { A.C > B }
s(s1(A, B)) :-
    a(A),
    b(B),
    a_cs(A, Cs),
    maplist(#<(B), Cs).

% A -> C D | C C C
a(a1(C, D)) :-
    c(C),
    d(D).
a(a2(C1, C2, C3)) :-
    c(C1),
    c(C2),
    c(C3).

% Leaf rules
b(B) :- B in 0..10.
c(C) :- C in 0..10.
d(D) :- D in 0..10.

% Field extractor for A.C
a_cs(a1(C, _D), [C]).
a_cs(a2(C1, C2, C3), [C1, C2, C3]).
