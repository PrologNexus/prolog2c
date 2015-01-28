%%% Support code for SWI Prolog


command_line_arguments(ARGS) :-
	current_prolog_flag(argv, X),
	append(_, ['--'|ARGS], X), !.

stream(X) :- blob(X, stream).

dbreference(_) :- fail.		% sufficient here

enable_trace(_).

current_error_output(S) :- current_output(S).

%% this must produce identical results as hash_name() in pc.h
hash_atom(A, H) :-
	atom_codes(A, AL),
	hash_atom(0, AL, 0, H).
hash_atom(I, AL, H, H) :-
	(I > 100; AL == []), !.
hash_atom(I, [C|R], H1, H2) :-
	H is (H1 xor ((H1 << 6) + (H1 >> 2) + C)) /\ 2147483647,
	I2 is I + 1,
	hash_atom(I2, R, H, H2).
