%%%% interpreter toplevel


:- include('version.pl').
:- include('lib/interp.pl').
:- include('dcg.pl').

main :-
	pi_init,
	command_line_arguments(ARGS),
	'$predicate_address'(dcg_rule/2, ADR),
	asserta((term_expansion((X --> Y), Z) :- '$call_predicate'(ADR, [(X --> Y), Z]))),
	parse_arguments(ARGS),
	!,
	((recorded(pi_initialization_goal, G); recorded(pi_default_initialization_goal, G))
	 -> call(G)
	 ; recorded(pi_silent, _, REF), erase(REF), repl).

repl :-
	display('?- '), flush,
	seeing(IN), telling(OUT),
	read(TERM, VARS), 
	(TERM == end_of_file, halt
	; compound(TERM), TERM = [_|_], consult_files(TERM)
	; catch(run_goal(TERM, VARS), EXN, (report_exception(EXN), see(IN), tell(OUT)))
	),
	!,			% force tailcall
	repl.
repl :-
	display('\nno.\n'),
	repl.

consult_files([]).
consult_files([F|R]) :- consult(F), consult_files(R).

run_goal(G, VARS) :-
	call(G),
	show_variables(VARS),
	display(' ? '), flush,
	(get_response -> fail; display('\nyes.\n')).

get_response :-
	get0(59), skip_line.

skip_line :-
	repeat, get0(10), !.

show_variables([]) :- !.
show_variables([NAME=X|MORE]) :-
	nl, display(NAME), display(' = '), writeq(X),
	show_variables(MORE).

report_exception(EXN) :-
	display('\nUncaught exception:\n'),
	writeq(EXN), nl.

parse_arguments([]).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments(['-version'|_]) :- show_version_and_exit.
parse_arguments(['-t'|MORE]) :-
	global_set(pi_trace_depth, 0),
	parse_arguments(MORE).
parse_arguments(['-q'|MORE]) :-
	recordz(pi_silent, yes),
	parse_arguments(MORE).
parse_arguments(['-i', G|MORE]) :-
	recordz(pi_default_initialization_goal, G),
	parse_arguments(MORE).
parse_arguments([FILENAME|_]) :-
	name(FILENAME, [45|_]), usage(1).
parse_arguments([FILENAME|MORE]) :-
	consult(FILENAME),
	parse_arguments(MORE).

usage(CODE) :-
	display('usage: pi [-version] [-q] [-h] [-t] [-i NAME] FILENAME ...\n'),
	halt(CODE).
