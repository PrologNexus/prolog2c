:- include('lib/interp.pl').

main :-
	consult('tests/0085-boyer.pl'),
	call(main).
