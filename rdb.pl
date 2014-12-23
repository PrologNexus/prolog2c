%%% record database operations


:- global_variable(record_db).
:- pre_initialization((foreign_call(db_create(record_db, 3001, DB)), set_global(record_db, DB))).

recorda(KEY, TERM) :- recorda(KEY, TERM, _).
recorda(KEY, TERM, REF) :- '$record'(KEY, TERM, REF, 0).
recordz(KEY, TERM) :- recordz(KEY, TERM, _).
recorda(KEY, TERM, REF) :- '$record'(KEY, TERM, REF, 1).

'$record'(KEY, TERM, REF, ATEND) :-
	get_global(record_db, DB),
	'$record_db_key'(KEY, KEY2),
	foreign_call(db_record(DB, ATEND, KEY2, TERM, REF)).

recorded(KEY, TERM) :- recorded(KEY, TERM, _).
recorded(KEY, TERM, REF) :-
	get_global(record_db, DB),
	'$record_db_key'(KEY, KEY2),
	foreign_call(db_find(DB, KEY2, REF1)),
	'$db_match'(REF1, TERM, REF).

'$db_match'(REF, TERM, REF) :-
	foreign_call(db_ref(REF, TERM)).
'$db_match'(REF1, TERM, REF) :-
	foreign_call(db_next(REF1, REF2)),
	'$db_match'(REF2, TERM, REF).
