%%% operations on terms


%% gather list of variables, assigning indices by unifying each var with '_var_'(INDEX)

index_variables(X, VARS) :- index_variables(X, 0, _, VARS).
					      
index_variables(X, C1, C2, [X]) :-
	var(X), !, X = '_var_'(C1), C2 is C1 + 1.

index_variables(X, C, C, []) :- atomic(X), !.

index_variables([], C, C, []).

index_variables([X|R], C1, C2, L) :-
	index_variables(X, C1, Cx, L1),
	index_variables(R, Cx, C2, L2),
	append(L1, L2, L), !.

index_variables(T, C1, C2, V) :-
	T =.. L, index_variables(L, C1, C2, V).

index_variables(_, C, C, []).


%% check if term contains variables

literal_term('_var_'(_)) :- !, fail.
literal_term([X|Y]) :-
	!, literal_term(X), literal_term(Y).
literal_term(X) :- atomic(X), !.
literal_term(X) :-
	X =.. LST,
	literal_term(LST).


%% collect instances of '_var_'(N) into lists of indexes

collect_indexed_variables('_var_'(N), [N]) :- !.
collect_indexed_variables(X, []) :- atomic(X), !.
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

map_indexed_variables_to_real_variables('_var_'(I), VLIST, V) :-
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
