%%% toplevel / driver for compiler
%
% - supported options:
%
%   -v                      show clauses are they are compiled
%   -h  -help  --help       show short usage description
%   -i                      show intermediate code instead of generating C
%   -o FILENAME             override output file name (default: <SOURCE_FILE>.c)
%   -I FILENAME             add FILENAME to include-path
%   -q                      disable any output
%
% Setting the environment variable PC_PRELUDE_FILE=<FILENAME> is
% equivalent to -L <FILENAME>.


:- initialization(main).

main :-
	command_line_arguments(ARGS),
	compile(ARGS),
	halt.

compile(ARGS) :-
	parse_arguments(ARGS),
	(recorded(source_file, FILE); default_setting(source_file, FILE)),
	default_setting(include_path, PATH),
	recorda(include_path, PATH),
	compile_file(FILE).

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
parse_arguments(['-q'|MORE]) :-
	recorda(silent, yes),
	parse_arguments(MORE).
parse_arguments(['-I', DIR|MORE]) :-
	recorded(include_path, OLD, REF),
	erase(REF),
	recorda(include_path, [DIR|OLD]),
	parse_arguments(MORE).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments([INFILE|MORE]) :-
	name(INFILE, IFL),
	IFL \= [45|_],		% 0'-
	file_name_string(IFL, OFL),
	append(OFL, ".c", OFL2),
	name(OUTFILE, OFL2),
	recorda(source_file, INFILE),
	(recorded(output_file, _) -> true; recorda(output_file, OUTFILE)),
	parse_arguments(MORE).
parse_arguments(_) :- usage(1).

usage(STATUS) :-
	gen('usage: pc [-h] [-v] [-o FILENAME] [-i] [FILENAME]\n'),
	halt(STATUS).
