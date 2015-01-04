%%% clause database operations


:- global_variable(clause_db).
:- pre_initialization((foreign_call(db_create(clause_db, 3001, DB)), set_global(clause_db, DB))).


asserta(TERM) :- asserta(TERM, _).
asserta(TERM, REF) :-
	!,
	'$clause_check_term'(TERM, HEAD, BODY),
	'$assert'(HEAD, BODY, 0, REF).
assertz(TERM) :- assertz(TERM, _).
assertz(TERM, REF) :-
	!,
	'$clause_check_term'(TERM, HEAD, BODY),
	'$assert'(HEAD, BODY, 1, REF).

'$assert'(HEAD, BODY, ATEND, REF) :-
	get_global(clause_db, DB),
	'$clause_db_key'(HEAD, KEY),
	!,
	foreign_call(db_record(DB, ATEND, KEY, (HEAD :- BODY), REF)).

'$clause_check_term'((HEAD :- BODY), HEAD, BODY) :- !.
'$clause_check_term'(HEAD, HEAD, true).

retract(TERM) :-
	!,
	get_global(clause_db, DB),
	'$clause_check_term'(TERM, HEAD, BODY),
	'$clause_db_key'(HEAD, KEY),
	foreign_call(db_find(DB, KEY, REF1)),
	'$retract_match'(REF1, (HEAD :- BODY), REF).

'$retract_match'(REF, TERM, REF) :-
	(foreign_call(db_ref(REF, X)) -> X = TERM, erase(REF)
	; garbage_collect, '$retract_match'(REF, TERM, REF)).
'$retract_match'(REF1, TERM, REF) :-
	foreign_call(db_next(REF1, REF2)),
	!,
	'$retract_match'(REF2, TERM, REF).

abolish(PI) :-
	get_global(clause_db, DB),
	abolish(PI, DB).

abolish([], DB) :- !.
abolish(NAME/ARITY, DB) :-
	!,
	'$clause_db_key'(NAME, ARITY, KEY),
	foreign_call(db_find(DB, KEY, REF)),
	foreign_call(db_erase_all(REF)).
abolish([PI|MORE], DB) :-
	!,
	abolish(PI, DB),
	abolish(MORE, DB).
abolish(NAME, DB) :-
	atom(NAME),
	foreign_call(db_find(DB, NAME, REF)),
	foreign_call(db_erase_all(REF)).

clause(HEAD, BODY) :- clause(HEAD, BODY, _).

clause(HEAD, BODY, REF) :-
	nonvar(REF),
	!,
	'$clause_match'(REF, (HEAD :- BODY), REF).
clause(HEAD, BODY, REF) :-
	get_global(clause_db, DB),
	'$clause_db_key'(HEAD, KEY),
	foreign_call(db_find(DB, KEY, REF1)),
	!,
	'$clause_match'(REF1, (HEAD :- BODY), REF).

'$clause_match'(REF, TERM, REF) :-
	(foreign_call(db_ref(REF, X)) -> X = TERM
	; garbage_collect, '$clause_match'(REF, TERM, REF)).
'$clause_match'(REF1, TERM, REF) :-
	foreign_call(db_next(REF1, REF2)),
	!,
	'$clause_match'(REF2, TERM, REF).

'$clause_db_key'(HEAD, KEY) :-
	functor(HEAD, NAME, ARITY),
	'$clause_db_key'(NAME, ARITY, KEY).
	
'$clause_db_key'(NAME, 0, NAME).
'$clause_db_key'(NAME, ARITY, K2) :-
	atom_codes(NAME, SNAME),
	number_codes(ARITY, SARITY),
	append(SNAME, [47|SARITY], SK),
	atom_codes(K2, SK).
