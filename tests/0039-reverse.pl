iota(N, L) :- iota(0, N, L).
iota(N, N, []).
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

main :-
	iota(100, L),
	display(L), nl,
	reverse(L, X),
	display(X), nl.

reverse(L, R) :- reverse(L, R, []).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).
