%%%% interpreter toplevel


:- include('lib/interp.pl').

main :-
	global_set(pi_trace_depth, none),
	command_line_arguments(ARGS),
	parse_arguments(ARGS),
	!,
	((recorded(initialization_goal, G); recorded(default_initialization_goal, G))
	 -> call(G)
	 ; banner, repl).

banner :-
	display('?-Prolog - (c)MMXV Felix L. Winkelmann'), nl.
	
repl :-
	display('?- '), flush,
	seeing(CURRENT),
	read(TERM, VARS), 
	(TERM == end_of_file, halt
	; catch(run_goal(TERM, VARS), EXN, (report_exception(EXN), see(CURRENT)))),
	!,			% force tailcall
	repl.
repl :-
	display('\nno.\n'),
	repl.

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
parse_arguments(['-h'|_]) :-
	usage(0).
parse_arguments(['-t'|MORE]) :-
	global_set(pi_trace_depth, 0),
	parse_arguments(MORE).
parse_arguments(['-i', G|MORE]) :-
	recordz(default_initialization_goal, G),
	parse_arguments(MORE).
parse_arguments([FILENAME|_]) :-
	name(FILENAME, [45|_]), usage(1).
parse_arguments([FILENAME|MORE]) :-
	consult(FILENAME),
	parse_arguments(MORE).

usage(CODE) :-
	display('usage: pi [-h] [-t] FILENAME ...\n'),
	halt(CODE).
