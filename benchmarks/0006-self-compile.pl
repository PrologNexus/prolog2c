:- include('settings.pl').
:- include('support.pl').
:- include('state.pl').
:- include('terms.pl').
:- include('index.pl').
:- include('dcg.pl').
:- include('macros.pl').
:- include('process.pl').
:- include('compile.pl').
:- include('assemble.pl').
:- include('xref.pl').

:- initialization(main).

main :-
	recordz(source_file, 'pc.pl'),
	recordz(library_dir, ['.']),
	recordz(output_file, '/dev/null'),
	recordz(silent, yes),
	compile_file('pc.pl').

