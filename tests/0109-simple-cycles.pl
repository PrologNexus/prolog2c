main :-
	A = f(A), B = f(B),
	\+acyclic_term(A),
	recordz(foo, A),
	recorded(foo, X), \+acyclic_term(X),
	A = X.
