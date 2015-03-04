%%% ISO stuff


atom_concat(X, Y, Z) :-
	var(X), !, name(Y, YL), atom_codes(Z, ZL), append(XL, YL, ZL), name(X, XL).
atom_concat(X, Y, Z) :-
	var(Y), !, name(X, XL), atom_codes(Z, ZL), append(XL, YL, ZL), name(Y, YL).
atom_concat(X, Y, Z) :-
	name(X, XL), name(Y, YL), append(XL, YL, ZL), atom_codes(Z, ZL).

atom_chars(X, Y) :- atom_codes(X, XL), findall(CH, (member(C, CL), char_code(CH, C)), Y).

number_chars(X, Y) :- number_codes(X, XL), findall(CH, (member(C, CL), char_code(CH, C)), Y).
