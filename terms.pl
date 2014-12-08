%%% operations on terms


%% gather list of variables, assigning indices by unifying each var with _var_(INDEX)

gather_variables(X, VARS) :- gather_variables(X, 0, _, VARS).
					      
gather_variables(X, C1, C2, [X]) :-
	var(X), !, X = '_var_'(C1), C2 is C1 + 1.

gather_variables(X, C, C, []) :- atomic(X), !.

gather_variables([], C, C, []).

gather_variables([X|R], C1, C2, L) :-
	gather_variables(X, C1, Cx, L1),
	gather_variables(R, Cx, C2, L2),
	append(L1, L2, L), !.

gather_variables(T, C1, C2, V) :-
	T =.. L, gather_variables(L, C1, C2, V).

gather_variables(_, C, C, []).


%% check if term contains variables

literal_term('_var_'(_)) :- !, fail.
literal_term([X|Y]) :-
	!, literal_term(X), literal_term(Y).
literal_term(X) :- atomic(X), !.
literal_term(X) :-
	X =.. LST,
	literal_term(LST).
