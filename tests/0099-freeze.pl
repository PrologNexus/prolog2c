main :-
	V = 99, freeze(V, (display(now), nl)),
	freeze(X, (display(fail), nl, fail)),
	Y = 1,
	X = 99,
	done.
main :-
	freeze(Z, (display([Z]), nl)),
	next(Z).

done :- display(done), nl.
done :- display(bad), nl.

next(123) :-
	display(next), nl.
