:- mode foo(+,-).

main :-
	foo([x,y], Z), display(Z), nl.

foo([X, Y], 1/Y).
