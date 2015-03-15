main :-
	atomic_list_concat([abc,'123.0',45,9.1,'___'], X), writeq(X), nl.

concatenate([], []).
concatenate([X|Y], Z) :-
	concatenate(Y, Z2),
	append(X, Z2, Z).

