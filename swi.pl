%%% Support code for SWI Prolog


:- op(400, yfx, '\\\\').

command_line_arguments(ARGS) :-
	current_prolog_flag(argv, X),
	append(_, ['--'|ARGS], X), !.

stream(X) :- blob(X, stream).

dbreference(_) :- fail.		% sufficient here

enable_trace(_).

current_error_output(S) :- current_output(S).
