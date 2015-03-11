%%%% utilities


iota(N, L) :- iota(0, N, L).
iota(N, N, []) :- !.
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

concatenate([], []).
concatenate([X|Y], Z) :-
	concatenate(Y, Z2),
	(atomic(X) -> name(X, XS); XS = X),
	append(XS, Z2, Z).

split_string(STR, SEP, [PART|P2]) :-
	append(PART, [SEP|MORE], STR),
	!,
	split_string(MORE, SEP, P2).
split_string(STR, _, [STR]).

chop(STR, STR2) :- chop(STR, 10, STR2).
chop(ATM, C, STR2) :- atom(ATM), atom_codes(ATM, STR), chop(STR, C, STR2).
chop(STR, C, STR2) :- append(STR2, [C], STR), !.
chop(STR, _, STR).
