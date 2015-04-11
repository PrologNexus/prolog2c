%%%% subprocess (client) API for "ensemble"


:- ensure_loaded(library(s11n)).
:- ensure_loaded(library(fd)).


write_message(FD, MSG) :-
	serialize_term(MSG, DATA),
	write_raw_message(FD, DATA).

read_message(FD, MSG) :-
	read_raw_message(FD, DATA),
	deserialize_term(DATA, MSG).

write_raw_message(FD, DATA) :-
	atom_length(DATA, LEN),
	C1 is LEN >> 16,
	C2 is (LEN >> 8) /\ 255,
	C3 is LEN /\ 255,
	write_bytes(FD, [C1, C2, C3]),
	write_bytes(FD, DATA).

read_raw_message(FD, DATA) :-
	read_bytes(FD, 3, [C1, C2, C3]),
	LEN is (C1 << 24) \/ (C2 << 16) \/ C3,
	read_bytes(FD, LEN, DATA).

mwrite(TERM) :-
	write_message(1, mwrite(TERM)).

mread(TERM) :-
	write_message(1, mread(TERM)),
	read_message(0, TERM).

mreadp(TERM) :-
	write_message(1, mreadp(TERM)),
	read_message(0, REPLY),
	!,
	REPLY = TERM.

mterminate :-
	write_message(1, mterminate).

mhalt :-
	write_message(1, mhalt).
