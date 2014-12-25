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
	number(X), ! number_codes(X, S).
name(X, S) :- atom_codes(X, S).


[X|Y] =.. Z :- !, Z = ['.', X, Y].
X =.. Y :- atomic(X), !, Y = [X].
X =.. [Y] :- !, atomic(Y), X = Y.
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

'$univ_args'(TERM, I, I, []) :- !.
'$univ_args'(TERM, I, N, [X|MORE]) :-
	I =< N,
	arg(I, TERM, X),
	I2 is I + 1,
	'$univ_args'(TERM, I2, N, MORE).
