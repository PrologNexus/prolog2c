:- global_variable(foo), global_variable(bar).

main :- 
	get_global(foo, X), display(X), nl,
	set_global(foo, foo(abc, def)),
	foreign_call(gc(0)),
	get_global(foo, Y), display(Y), nl.
