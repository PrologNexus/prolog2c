nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).

main :-
	X = 123, display(X), nl,
	[A,1] = [1,B], display(A), display(B), nl.
