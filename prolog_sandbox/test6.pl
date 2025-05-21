:- use_module(library(clpfd)).

% S -> A B { A.E.C > B }
s(s1(A, B)) :-
    a(A),
    b(B),
    a_cs(A, Cs),           
    maplist(#<(B), Cs).    

% A -> E E
a(a1(E1, E2)) :-
    e(E1),
    e(E2).

% E - C D | C C C | D 
e(e1(C, D)) :- c(C), d(D).
e(e2(C1, C2, C3)) :- c(C1), c(C2), c(C3).
e(e3(D)) :- d(D).

% Leaf rules
b(B) :- B in 0..10.
c(C) :- C in 0..10.
d(D) :- D in 0..10.

% Field extractor for A.E.C
a_cs(a1(E1, E2), Cs) :-
    e_cs(E1, Cs1),
    e_cs(E2, Cs2),
    append(Cs1, Cs2, Cs).

% Field extractor for E.C
e_cs(e1(C, _), [C]).
e_cs(e2(C1, C2, C3), [C1, C2, C3]).
e_cs(e3(_), []).

% Term generation
example_s(S) :-
    s(S),
    extract_vars(S, Vars),
    labeling([], Vars).

extract_vars(s1(A, B), Vars) :-
    a_vars(A, AVars),
    Vars = [B | AVars].

a_vars(a1(E1, E2), Vars) :-
    e_vars(E1, Vars1),
    e_vars(E2, Vars2),
    append(Vars1, Vars2, Vars).

e_vars(e1(C, D), [C, D]).
e_vars(e2(C1, C2, C3), [C1, C2, C3]).
e_vars(e3(D), [D]).

