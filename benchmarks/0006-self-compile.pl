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

atomic_list_concat(AL, A) :-
	findall(AA, (member(A, AL), name(A, AA)), LL),
	concatenate(LL, L),
	atom_codes(A, L).

:- initialization(main).

main :-
	recordz(source_file, 'pc.pl'),
	recordz(include_path, ['.']),
	recordz(output_file, '/dev/null'),
	recordz(silent, yes),
	compile_file('pc.pl').

