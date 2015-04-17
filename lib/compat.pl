%%%% compatibility predicates for SWI


:- mode gensym(+, -).

:- global_variable(gensym_counter).
:- pre_initialization(global_set(gensym_counter, 1)).


gensym(BASE, ATM) :-
	global_ref(gensym_counter, C),
	C2 is C + 1,
	global_set(gensym_counter, C2),
	atomic_list_concat([BASE, C], ATM).

atom_number(A, N) :-
	var(A),
	!,
	number_codes(NC, N),
	atom_codes(A, NC).
atom_number(A, N) :-
	atom_codes(A, AC),
	number_codes(N, AC).

%%XXX dog slow and incomplete
char_type(C, ascii) :- between(0, 127, C).
char_type(C, ctrl) :- between(0, 31, C).
char_type(C, space) :- member(C, [9, 10, 11, 12, 13, 32]).
char_type(C, lower) :- member(C, "abcdefghijklmnopqrstuvwxyz").
char_type(C, upper) :- member(C, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").
char_type(C, alpha) :- char_type(C, lower); char_type(C, upper).
char_type(C, digit) :- member(C, "0123456789").
char_type(C, alnum) :- char_type(C, alpha); char_type(C, digit).
char_type(-1, end_of_file).
char_type(C, white) :- C = 9; C = 32.
