%%%% pathname operations


canonical_path(IN, OUT) :-
	absolute_pathname(IN, ALST),
	split_string(ALST, "/", "", OUTLST),
	canonicalize_pathname(OUTLST, CLST),
	append(CLST, LST),
	atom_codes(OUT, LST).

canonicalize_pathname([], []).
canonicalize_pathname(["."|R], R2) :- canonicalize_pathname(R, R2).
canonicalize_pathname([X, ".."|R], R2) :- canonicalize_pathname(R, R2).
canonicalize_pathname([X|R], [[47|X]|R2]) :- canonicalize_pathname(R, R2).

absolute_pathname(FNAME, ANAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	absolute_pathname(FLST, ANAME).
absolute_pathname([47|R], [47|R]).
absolute_pathname(LST, OLST) :-
	getcwd(PWD),
	atom_codes(PWD, PLST),
	append([PLST, "/", LST], OLST).

basename(FNAME, BNAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	basename(FLST, BNAME).
basename(FNAME, BNAME) :-
	append(_, BNAME, FNAME),
	\+memberchk(47, BNAME), !.

dirname(FNAME, DNAME) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	dirname(FLST, DNAME).
dirname(FNAME, DNAME) :-
	append(DNAME, [47|R], FNAME),
	\+memberchk(47, R), !.

suffix(FNAME, SUF) :-
	atom(FNAME), atom_codes(FNAME, FLST),
	suffix(FLST, SUF).
suffix(FNAME, SUF) :-
	append(_, SUF, FNAME),
	\+memberchk(46, SUF), !,
	FNAME \== SUF.
