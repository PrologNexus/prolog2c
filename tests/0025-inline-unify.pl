main :- 123 = 123,
	X = X,
	U = V,
	Y = 123,
	Z = Y, display('1\n'),
	X = Y, display('2\n'),
	X = 123, display('3\n'),
	display([X, Y, Z, U, V]), nl,
	f(X) = f(123).

nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).

