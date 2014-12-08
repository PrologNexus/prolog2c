%%% toplevel / driver for compiler
%
% - supported options:
%
%   -v                      show clauses are they are compiled
%   -h  -help  --help       show short usage description
%   -i                      show intermediate code instead of generating C
%   -o FILENAME             override output file name (default: <SOURCE_FILE>.c)
%
% Setting the environment variable PC_PRELUDE_FILE=<FILENAME> is
% equivalent to -L <FILENAME>.


:- include('all.pl').
:- initialization(main).

main :-
	(getenv('PC_PRELUDE_FILE', FNAME) -> recordz(library_files, FNAME); true),
	command_line_arguments(ARGS),
	parse_arguments(ARGS),
	(recorded(source_file, FILE); default_setting(source_file, FILE)),
	compile_file(FILE),
	halt.

parse_arguments([]).
parse_arguments(['-o', OFILE|MORE]) :-
	recorda(output_file, OFILE),
	parse_arguments(MORE).
parse_arguments(['-i'|MORE]) :-
	recorda(show_intermediate_code, yes),
	parse_arguments(MORE).
parse_arguments(['-v'|MORE]) :-
	recorda(show_compiled_clauses, yes),
	parse_arguments(MORE).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments([INFILE|MORE]) :-
	name(INFILE, IFL),
	file_name_string(IFL, OFL),
	append(OFL, ".c", OFL2),
	name(OUTFILE, OFL2),
	recorda(source_file, INFILE),
	(recorded(output_file, _) -> true; recorda(output_file, OUTFILE)),
	parse_arguments(MORE).
parse_arguments(_) :- usage(1).

usage(STATUS) :-
	gen('usage: pc [-h] [-v] [-o FILENAME] [-i] [-L FILENAME]* [FILENAME]\n'),
	halt(STATUS).
