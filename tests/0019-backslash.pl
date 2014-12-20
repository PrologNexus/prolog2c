nl :- foreign_call(write_char(10)).
display(X) :- foreign_call(basic_write(X)).

main:-
	display('this\tis \"a\"\ntest\n'),
	display("\'list\\\n"), nl.
