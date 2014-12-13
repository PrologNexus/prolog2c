
display(X) :- foreign_call(basic_write(X)).

main :-
	(\+true; display('1 ok\n')),
	\+fail, display('2 ok\n'),
	foo.

% note: SWI prints "foo", which is not ISO compliant
foo :- \+(!).
foo :- display('foo\n').
