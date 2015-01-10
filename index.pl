%%% clause indexing


%% create initial state

initial_clause_indexing_state(s(no, no, no)). % 1st and 2nd not used in the moment


%% build map from clause-indices to types (clauses must have arity > 0)

build_index_to_type_map([], _, [], S, S).
build_index_to_type_map([(HEAD :- _)|MORE], I, [I/T1|ENTRIES], S1, S2) :-
        !,
	(HEAD =.. [_, A1|_] -> get_argument_type(A1, T1); T1 = none),
	I2 is I + 1,
	build_index_to_type_map(MORE, I2, ENTRIES, S1, S2).
build_index_to_type_map([HEAD|MORE], I, ENTRIES, S1, S2) :-	
	build_index_to_type_map([(HEAD :- true)|MORE], I, ENTRIES, S1, S2).

get_argument_type(X, var) :- var(X), !.
get_argument_type([], null).
get_argument_type([_|_], pair).
get_argument_type(X, integer(X)) :- integer(X), !.
get_argument_type(X, float) :- number(X), !.
get_argument_type(X, atom(X)) :- atom(X), !.
get_argument_type(X, structure) :- compound(X), !.


%% compile indexing instructions

compile_clause_indexing(_, _, [_/none|_], XS, XS, S, S). % arity 0 - nothing to do
compile_clause_indexing(_, _, [_/var|_], XS, XS, S, S). % first case is var
compile_clause_indexing(N, A, MAP, XS1, XS2, S1, S2) :-
	scan_indexing_types(MAP, [], DMAP),
	compile_dispatch(DMAP, N, A, XS1, XS2, S1, S2).


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

compile_dispatch(DMAP, N, A, XS1, XS2, S1, S2) :-
	(select(I/integer(_), DMAP, DMAP2) % we must be sure to test for fixnum first
	; memberchk(I/var, DMAP), DMAP2 = DMAP
	),
	!,
	secondary_clause_label(N, A, I, L),
	emit(switch_on_integer(L)),
	compile_dispatch_sequence(DMAP2, N, A, XS1, XS2, S1, S2).
compile_dispatch(DMAP, N, A, s(X, Y, no), XS2, S1, S2) :-
	gen_label(FL, S1, S3),
	gen_label(L2, S3, S4),
	emit(switch_on_noninteger(L2), label(FL), no_redo, fail, label(L2)),
	compile_dispatch_sequence(DMAP, N, A, s(X, Y, FL), XS2, S4, S2).
compile_dispatch(DMAP, N, A, XS1, XS2, S1, S2) :-
	XS1 = s(_, _, FL),
	emit(switch_on_integer(FL)),
	compile_dispatch_sequence(DMAP, N, A, XS1, XS2, S1, S2).

compile_dispatch_sequence([], _, _, s(X, Y, no), s(X, Y, FL), S1, S2) :-
	gen_label(FL, S1, S2),
	emit(label(FL), no_redo, fail).
compile_dispatch_sequence([], _, _, XS, XS, S, S) :- % re-use fail-point
	XS = s(_, _, FL),
	emit(jump(FL)).
compile_dispatch_sequence([I/var], N, A, XS, XS, S, S) :-
	secondary_clause_label(N, A, I, L),
	emit(jump(L)).
compile_dispatch_sequence([I/T|DMAP], N, A, XS1, XS2, S1, S2) :-
	dispatch_instruction(T, INSTNAME),
	secondary_clause_label(N, A, I, L),
	INST =.. [INSTNAME, L],
	emit(INST),
 	compile_dispatch_sequence(DMAP, N, A, XS1, XS2, S1, S2).

dispatch_instruction(var, switch_on_var).
dispatch_instruction(null, switch_on_null).
dispatch_instruction(pair, switch_on_pair).
dispatch_instruction(float, switch_on_float).
dispatch_instruction(atom(_), switch_on_atom).
dispatch_instruction(structure, switch_on_structure).
