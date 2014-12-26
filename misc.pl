%%% miscellaneous predicates


compare(>, X, Y) :- X @> Y, !.
compare(<, X, Y) :- X @< Y, !.
compare(=, X, X).


%% this is just a fake, to have some sort of error-signalling operation

throw(EXN) :-
	foreign_call(current_error_stream(S)),
	foreign_call(set_current_output_stream(S)),
	display('\nERROR: '), display(EXN), nl,
	halt(70).

shell(CMD) :- shell(CMD, 0).

name(X, S) :-
	var(X), !,
	(number_codes(X, S); atom_codes(X, S)).
name(X, S) :-
	number(X), !, number_codes(X, S).
name(X, S) :- atom_codes(X, S).


X =.. Y :- atomic(X), !, Y = [X].
X =.. Y :-
	compound(X), !,
	functor(X, NAME, ARITY),
	Y = [NAME|ARGS],
	'$univ_args'(X, 1, ARITY, ARGS).
X =.. [N|ARGS] :-
	var(X),
	length(ARGS, ARITY),
	functor(X, N, ARITY),
	'$univ_args'(X, 1, ARITY, ARGS).

'$univ_args'(TERM, I, N, []) :- I > N, !.
'$univ_args'(TERM, I, N, [X|MORE]) :-
	I =< N,
	arg(I, TERM, X),
	I2 is I + 1,
	'$univ_args'(TERM, I2, N, MORE).

%XXX inefficient, not steadfast, and probably broken
atomic_list_concat(LIST, ATOM) :-
	'$atomic_list_concat'(LIST, ALL),
	atom_codes(LIST, ALL).
'$atomic_list_concat'([], []) :- !.
'$atomic_list_concat'([X|R], ALL) :-
	name(X, XLIST),
	'$atomic_list_concat'(R, REST),
	append(XLIST, REST, ALL).
