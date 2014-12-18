display(X) :- foreign_call(basic_write(X)).
nl :- foreign_call(write_char(10)).
member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

main :-
	member(X, [1,2,3]),
        member(Y, [a,b,c]),
	display(X/Y), nl,
	fail.
main.

