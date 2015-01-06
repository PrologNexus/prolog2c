%%% support code for findall/3


:- global_variable(findall_solutions).
:- pre_initialization(global_set(findall_solutions, [])).

findall_start :- findall_push('$<mark>').

findall_push(X) :-
	!,
	global_ref(findall_solutions, L),
	copy_term(X, X2),
	deref_term([X2|L], 2, X3),
	global_set(findall_solutions, X3).

findall_collect(L) :-
	global_ref(findall_solutions, SL),
	findall_collect(SL, [], L).

findall_collect([X|MORE], R, L) :-
	(var(X); X \== '$<mark>'),
	!, findall_collect(MORE, [X|R], L).
findall_collect([_|MORE], R, R) :-
	!, global_set(findall_solutions, MORE).
