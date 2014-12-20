display(X) :- foreign_call(basic_write(X)).
nl :- foreign_call(write_char(10)).

main :-
	iota(1000, X),
	foreign_call(gc(0)),
	dump(X).

iota(N, L) :- iota(0, N, L).
iota(N, N, []) :- !.
iota(N, M, [N|R]) :-
	!, N2 is N + 1, iota(N2, M, R).

dump([]) :- nl.
dump([X|Y]) :-
	!, display(X), display(' '), dump(Y).
