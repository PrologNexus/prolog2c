iota(N, L) :- iota(0, N, L).
iota(N, N, []).
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

main :-
	union([1,2,3,4,5],[3,4,5,6,7],X),
	display(X), nl,
	intersection([1,2,3,4,5],[3,4,5,6,7],Y),
	display(Y), nl,
	difference([1,2,3,4,5],[3,4,5,6,7],Z),
	display(Z), nl.

union([], X, X).
union([X|R], Y, Z):- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]):- union(R, Y, Z).

intersection([], X, []).
intersection([X|R], Y, [X|Z]) :- member(X, Y), !, intersection(R, Y, Z).
intersection([X|R], Y, Z) :- intersection(R, Y, Z).

difference([], _, []) :- !.
difference([A|C], B, D) :-
        member(A, B), !,
        difference(C, B, D).
difference([A|B], C, [A|D]) :-
        difference(B, C, D).
