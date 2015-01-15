%%% simple test for embedding


main :-
	write('one\n'),
	suspend(1, X),
	write(' -> '), write(X), nl, X =:= 123,
	write('two\n'),
	suspend(2, Y),
	write(' -> '), write(Y), nl, Y =:= 456,
	halt.
