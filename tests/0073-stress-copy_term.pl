main :-
	length(L, 100),
	loop(L).

loop(L) :- repeat, copy_term(L, L2), length(L2, 100), fail.

