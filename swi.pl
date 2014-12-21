%%%% Support code for SWI Prolog


command_line_arguments(ARGS) :-
	current_prolog_flag(argv, X),
	append(_, ['--'|ARGS], X), !.

file_exists(NAME) :- exists_file(NAME); exists_directory(NAME).
