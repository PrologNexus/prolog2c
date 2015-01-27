%%% clause indexing


%% build map from clause-indices to types (clauses must have arity > 0)

build_index_to_type_map([], _, []).
build_index_to_type_map([(HEAD :- _)|MORE], I, [I/T1|ENTRIES]) :-
        !,
	(HEAD =.. [_, A1|_] -> get_argument_type(A1, T1); T1 = none),
	I2 is I + 1,
	build_index_to_type_map(MORE, I2, ENTRIES).
build_index_to_type_map([HEAD|MORE], I, ENTRIES) :-	
	build_index_to_type_map([(HEAD :- true)|MORE], I, ENTRIES).

get_argument_type(X, var) :- var(X), !.
get_argument_type([], null).
get_argument_type([_|_], pair).
get_argument_type(X, integer(X)) :- integer(X), !.
get_argument_type(X, float) :- number(X), !.
get_argument_type(X, atom(X)) :- atom(X), !.
get_argument_type(X, structure) :- compound(X), !.


%% compile indexing instructions

compile_clause_indexing(_, _, [_/none|_], S, S). % arity 0 - nothing to do
compile_clause_indexing(_, _, [_/var|_], S, S). % first case is var
compile_clause_indexing(N, A, MAP, S1, S2) :-
	scan_indexing_types(MAP, [], DMAP),
	compile_dispatch(DMAP, N, A, S1, S2).


%% collect list of types to dispatch on

scan_indexing_types([], _, []).
scan_indexing_types([I/var|_], _, [I/var]). % variable will match anything
scan_indexing_types([_/T|MORE], DONE, R) :- % type already seen
	member_in_indexing_types(T, DONE),
	scan_indexing_types(MORE, DONE, R).
scan_indexing_types([I/T|MORE], DONE, [I/T|R]) :-
	scan_indexing_types(MORE, [T|DONE], R).

member_in_indexing_types(_, []) :- !, fail.
member_in_indexing_types(T, [T|_]) :- !.
member_in_indexing_types(integer(_), [integer(_)|_]) :- !.
member_in_indexing_types(atom(_), [atom(_)|_]) :- !.
member_in_indexing_types(T, [_|MORE]) :- member_in_indexing_types(T, MORE).
			 

%% compile code to dispatch on type

compile_dispatch(DMAP, N, A, S1, S2) :-
	%% remaining clauses contain integer or var case
	(select(I1/integer(_), DMAP, DMAP2)
	; memberchk(I1/var, DMAP), DMAP2 = DMAP
	),
	!,
	DMAP = [I2/_|_],
	secondary_clause_label(N, A, I1, L1),
	secondary_clause_label(N, A, I2, L2),
	%% test for fixnum first, otherwise run first clause if arg is a var
	emit(switch_on_integer(L1), switch_on_var(L2)),
	compile_dispatch_sequence(DMAP2, N, A, s(no), S1, S2).
compile_dispatch(DMAP, N, A, S1, S2) :-
	%% no integer or var case
	gen_label(FL, S1, S3),
	gen_label(L1, S3, S4),
	DMAP = [I/_|_],
	secondary_clause_label(N, A, I, L2),
	%% integer fails, var runs first clause, and create fail-block
	emit(switch_on_integer(FL), switch_on_var(L2), jump(L1)),
	emit(label(FL), no_redo, fail, label(L1)),
	compile_dispatch_sequence(DMAP, N, A, s(FL), S4, S2).

compile_dispatch_sequence([], _, _, s(no), S, S) :-
	emit(no_redo, fail).
compile_dispatch_sequence([], _, _, s(FL), S, S) :- % re-use fail-point
	emit(jump(FL)).
compile_dispatch_sequence([I/var], N, A, _, S, S) :-
	secondary_clause_label(N, A, I, L),
	emit(jump(L)).
compile_dispatch_sequence([I/T|DMAP], N, A, XS, S1, S2) :-
	dispatch_instruction(T, INSTNAME),
	secondary_clause_label(N, A, I, L),
	INST =.. [INSTNAME, L],
	emit(INST),
 	compile_dispatch_sequence(DMAP, N, A, XS, S1, S2).

%% integer already handled
dispatch_instruction(var, switch_on_var).
dispatch_instruction(null, switch_on_null).
dispatch_instruction(pair, switch_on_pair).
dispatch_instruction(float, switch_on_float).
dispatch_instruction(atom(_), switch_on_atom).
dispatch_instruction(structure, switch_on_structure).
