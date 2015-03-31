%%%% subprocess invocation (Prolog part)


:- verbatim('#include "spawn.c"').


%% run subprocess CMD, with list of arguments (not including command)
%
% unifies P with process(PID, FDIN, FDOUT)

spawn(CMD, ARGS, P) :-
	foreign_call(spawn(CMD, ARGS, PID, FDIN, FDOUT)),
	P = process(PID, FDIN, FDOUT),
	(recorded(spawned_children, PS, REF), erase(REF); PS = []),
	!, recordz(spawned_children, [P|PS]).


%% call this periodically to reap zombie children

reap_children :-
	foreign_call(reap_children(PID)),
	recorded(spawned_children, PS, REF),
	select(process(PID, FDIN, FDOUT), PS, NEWPS), % may fail, if unregistered process terminates
	erase(REF),
	foreign_call(close_fd(FDIN)),
	foreign_call(close_fd(FDOUT)),
	recordz(spawned_children, NEWPS),
	reap_children.
reap_children.


%% send atom(ic) or string to subprocess, forces all data to be written

send(P, DATA) :-
	atomic(DATA), DATA \= [],
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

%% probably generally useful, find out how this is usually called
skip_items(0, _, []).
skip_items(N, [_|R], R2) :-
	N2 is N - 1,
	!, skip_items(N2, R, R2).


%% read string from subprocess, may read fewer than LEN bytes
%
% will return [] on EOF

receive(process(_, FDIN, _), LEN, BYTES) :-
	foreign_call(raw_read(FDIN, LEN, BYTES)).
