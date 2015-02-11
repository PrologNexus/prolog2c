%%%% make - only usable in interpreter


:- op(1050, xfy, ':').

make(RS) :- RS = [(T : _ -> _)|_], make(RS, T).

make(RS, T) :-
	make1(RS, T), report_made.

report_made :-
	recorded(made, X, REF), erase(REF),
	display('% make: made '), display(X), nl, fail.
report_made.

make1(RS, T) :-
	atom(T) -> make1(RS, RS, T); forall(member(T1, T), make1(RS, RS, T1)).

make1([], _, T) :-
	exists_file(T); throw(error('don\'t know how to make', T)).
make1([(T : DS -> _)|_], RS, T) :-
	check_deps(T, DS, RS), exists_file(T).
make1([(T : _ -> CMD)|_], RS, T) :-
	!, display('% make: making '), display(T), nl,
	recorda(made, T), call(CMD).
make1([_|R], RS, T) :- make1(R, RS, T).

check_deps(T, (D1, D2), RS) :-
	!,
	check_deps(T, D1, RS),
	check_deps(T, D2, RS).
check_deps(T, D, RS) :-
	make1(RS, RS, D),
	file_modification_time(T, TM),
	file_modification_time(D, DTM),
	!, DTM < TM.
