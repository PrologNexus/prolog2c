:- include('lib/interp.pl').

main :-
	consult('tests/0060-crypta.pl'),
	call(main).
