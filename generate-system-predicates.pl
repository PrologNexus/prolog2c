%%% generate include-files for interp.pl from list of system-predicates


main :-
	op(200, fy, ['#', '?']),
	tell('system_predicate.pl'),
	telling(SP),
	tell('call_primitive.pl'),
	telling(CP),
	run(SP, CP),
	tell(SP), told,
	tell(CP), told.

run(SP, CP) :-
	repeat,
	read(TERM),
	gen_sys_pred(TERM, SP),
	gen_call_prim(TERM, CP),
	TERM == end_of_file.

gen_sys_pred(DEF, SP) :-
	system_predicate_head(DEF, PRED),
	!,
	functor(PRED, NAME, ARITY),
	tell(SP),
	writeq(system_predicate(NAME, ARITY)), put(46), nl.
gen_sys_pred(_, _).

gen_call_prim(DEF, CP) :-
	system_predicate_head(DEF, PRED),
	!,
	functor(PRED, NAME, ARITY),
	build_lists(1, ARITY, TERM, ARGS, CALLARGS),
	CALL =.. [NAME|CALLARGS],
	tell(CP),		    
	write((call_primitive(NAME, ARITY, TERM) :- !, ARGS, CALL)),
	display('.\n').	
gen_call_prim(_, _).

build_lists(I, N, _, true, []) :- I > N, !.
build_lists(I, N, T, (arg(I, T, X), VARS), [X|ARGS]) :-
	I2 is I + 1,
	build_lists(I2, N, T, VARS, ARGS).

system_predicate_head(system_predicate(PRED), PRED).
system_predicate_head(system_predicate(PRED, _), PRED).
system_predicate_head(system_predicate(PRED, _, _), PRED).

