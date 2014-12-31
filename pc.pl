%%% Support code for compiling PC with PC


atomic_list_concat(AL, A) :-
	findall(AA, (member(A, AL), name(A, AA)), LL),
	concatenate(LL, L),
	atom_codes(A, L).
