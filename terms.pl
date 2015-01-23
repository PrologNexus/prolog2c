%%% operations on terms


%% gather list of variables, assigning indices by unifying each var with '_var_'(INDEX),
%% also returns list of variable-indices that are referenced multiple times

index_variables(X, VARS, REFD) :-
	index_variables(X, 0, _, VARS, REFD).
index_variables(X, C1, C2, [X], []) :-
	var(X), !, indexed_variable(X, C1), C2 is C1 + 1.
index_variables(X, C, C, [], [N]) :-
	indexed_variable(X, N), !.
index_variables(X, C, C, [], []) :- atomic(X), !.
index_variables([X|R], C1, C2, L, REFD) :-
	index_variables(X, C1, Cx, L1, REFD1),
	index_variables(R, Cx, C2, L2, REFD2),
	append(L1, L2, L),
	union(REFD1, REFD2, REFD),
	!.
index_variables(T, C1, C2, V, REFD) :-
	T =.. L, index_variables(L, C1, C2, V, REFD).


%% wrapping and unwrapping of indexed variables - we must take care not to
%% have direct occurrences of '_var_'(N), or they may be compiled incorrectly

indexed_variable(X, I) :- X =.. ['_var_', I].


%% check if term contains variables and fail if it does

literal_term(X) :- indexed_variable(X, _), !, fail.
literal_term([X|Y]) :-
	!, literal_term(X), literal_term(Y).
literal_term(X) :- atomic(X), !.
literal_term(X) :-
	X =.. LST,
	literal_term(LST).


%% collect instances of '_var_'(N) into lists of indexes

collect_indexed_variables(X, [N]) :-
	indexed_variable(X, N), !.
collect_indexed_variables(X, []) :-
	atomic(X), !.
collect_indexed_variables(X, NS) :-
	X =.. [_|ARGS],
	collect_indexed_variables(ARGS, [], NS).

collect_indexed_variables([], NS, NS).
collect_indexed_variables([X|Y], NS1, NS) :-
	collect_indexed_variables(X, NS2),
	union(NS1, NS2, NS3),
	collect_indexed_variables(Y, NS3, NS).


%% using a map of variable-indices and real variables, build a new term with instances
%% of '_var_'(N) replaced by the real variables

map_indexed_variables_to_real_variables(X, VLIST, V) :-
	indexed_variable(X, I),
	!,
	member(I/V, VLIST).
map_indexed_variables_to_real_variables(X, _, X) :-
	atomic(X),
	!.
map_indexed_variables_to_real_variables([X|Y], VLIST, [Z|U]) :-
	!,
	map_indexed_variables_to_real_variables(X, VLIST, Z),
	map_indexed_variables_to_real_variables(Y, VLIST, U).
map_indexed_variables_to_real_variables(X, VLIST, Y) :-
	X =.. [N|ARGS],
	map_indexed_variables_to_real_variables(ARGS, VLIST, ARGS2),
	Y =.. [N|ARGS2].


%% collect list of variables - this doesn't use findall/3, to avoid renaming

find_unbound_variables([], []).
find_unbound_variables([_/V|MORE], [V|REST]) :-
	find_unbound_variables(MORE, REST).
