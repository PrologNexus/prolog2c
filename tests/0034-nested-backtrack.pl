member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

main :-
	member(X, [1,2,3]),
        member(Y, [a,b,c]),
	display(X/Y), nl,
	fail.
main.

