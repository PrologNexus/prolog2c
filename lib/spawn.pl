%%%% subprocess invocation (Prolog part)


:- verbatim('#include "spawn.c"').


spawn(CMD, ARGS, P) :-
	foreign_call(spawn(CMD, ARGS, PID, FDIN, FDOUT)),
	P = process(PID, FDIN, FDOUT),
	(recorded(spawned_children, PS, REF), erase(REF); PS = []),
	!, recordz(spawned_children, [P|PS]).

%% call this periodically
reap_children :-
	foreign_call(reap_children(PID)),
	recorded(spawned_children, PS, REF),
	select(process(P, FDIN, FDOUT), PS, NEWPS), % may fail, if unregistered process terminates
	erase(REF),
	foreign_call(close_fd(FDIN)),
	foreign_call(close_fd(FDOUT)),
	recordz(spawned_children, NEWPS),
	reap_children.
reap_children.

%% forces all data to be written
send(P, DATA) :-
	atomic(DATA),
	!,
	name(DATA, LST),
	send(P, LST).
send(process(_, _, FDOUT), DATA) :-
	send_block(FDOUT, DATA).

send_block(_, []) :- !.
send_block(FD, DATA) :-
	foreign_call(raw_write(FD, DATA, N)),
	skip_items(N, DATA, REST),
	!, send_block(FD, REST).

skip_items(0, _, []).
skip_items(N, [_|R], R2) :-
	N2 is N - 1,
	!, skip_items(N2, R, R2).

%% may read fewer than LEN bytes
receive(process(_, FDIN, _), LEN, BYTES) :-
	foreign_call(raw_read(FDIN, LEN, BYTES)).
