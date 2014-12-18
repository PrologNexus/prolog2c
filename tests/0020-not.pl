
display(X) :- foreign_call(basic_write(X)).

main :-
	(\+true; display('1 ok\n')),
	\+fail, display('2 ok\n'),
	foo.

foo :- \+(!).
foo :- display('foo\n').
