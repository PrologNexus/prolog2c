%%% Wrapper for compiling compiler with itself


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

atomic_list_concat(AL, A) :-
	findall(AA, (member(A, AL), name(A, AA)), LL),
	concatenate(LL, L),
	atom_codes(A, L).

show_version_and_exit :-
	current_prolog_flag(version, V),
	current_prolog_flag(prolog_title, T),
	current_prolog_flag(prolog_copyright, C),
	display(T), display(' version '), display(V), display(' - '),
	display(C), nl, halt.

:- include('main.pl').
