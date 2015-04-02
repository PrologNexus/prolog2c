%%%% subprocess invocation (Prolog part)


:- verbatim('#include "spawn.c"').
:- ensure_loaded(library(fd)).


%% run subprocess CMD, with list of arguments (not including command)
%
% unifies P with process(PID, FDIN, FDOUT)

spawn(CMD, ARGS, P) :-
	foreign_call(spawn(CMD, ARGS, PID, FDIN, FDOUT)),
	P = process(PID, FDIN, FDOUT),
	(recorded(spawned_children, PS, REF), erase(REF); PS = []),
	!, recordz(spawned_children, [P|PS]).


%% disconnect from child process orderly

disconnect(process(PID, FDIN, FDOUT)) :-
	recorded(spawned_children, PS, REF),
	select(process(PID, FDIN, FDOUT), PS, NEWPS), % may fail, if process is already disconnected
	erase(REF),
	close_fd(FDIN),
	close_fd(FDOUT),
	recordz(spawned_children, NEWPS).
disconnect(_).


%% call this periodically to reap zombie children

reap_children :-
	foreign_call(reap_children(PID)),
	disconnect(process(PID, _, _)).
	reap_children.
reap_children.


%% send atom(ic) or string to subprocess, forces all data to be written

send(P, DATA) :-
	atomic(DATA), DATA \= [],
	!,
	name(DATA, LST),
	send(P, LST).
send(process(_, _, FDOUT), DATA) :-
	write_bytes(FDOUT, DATA).

send_block(FD, DATA) :-
	write_bytes(FD, DATA).


%% read string from subprocess, may read fewer than LEN bytes
%
% will return [] on EOF, and block after that

receive(process(_, FDIN, _), LEN, BYTES) :-
	read_bytes(FDIN, LEN, BYTES).
