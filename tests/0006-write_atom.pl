nl :- foreign_call(write_char(10)).

main :- foreign_call(basic_write(65)), nl,
	foreign_call(basic_write([])), nl,
	foreign_call(basic_write(abc)), nl,
	foreign_call(basic_write(X)), nl.
