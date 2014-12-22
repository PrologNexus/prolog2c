:- global_variable(foo), global_variable(bar).

main :- 
	get_global(foo, X), display(X), nl,
	set_global(foo, foo(abc, def)),
	garbage_collect,
	get_global(foo, Y), display(Y), nl.
