main :-
	sub_atom(hello, B, L, A, X),
	writeq([B, X, L, A]), nl,
	fail.


sub_atom(X, _, _, _, _) :-
	var(X), throw(instantiation_error).
sub_atom(ATOM, BEFORE, LEN, AFTER, SATOM) :-
	atom_length(ATOM, ALEN),
	ALEN2 is ALEN - 1,
	between(0, ALEN, BEFORE),
	between(0, ALEN, AFTER),
	LEN is ALEN - AFTER - BEFORE,
	LEN >= 0,
	foreign_call(sub_atom(ATOM, BEFORE, LEN, SATOM)).
