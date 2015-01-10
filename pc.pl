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

atomic_list_concat(AL, A) :-
	findall(AA, (member(A, AL), name(A, AA)), LL),
	concatenate(LL, L),
	atom_codes(A, L).

:- include('main.pl').
