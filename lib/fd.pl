%%%% operations on file-descriptors and raw I/O


:- verbatim('#include "fd.c"').


write_bytes(_, []) :- !.
write_bytes(_, '') :- !.
write_bytes(FD, DATA) :-
	atom(DATA),
	!,
	foreign_call(raw_write(FD, DATA, N)),
	sub_atom(DATA, N, LEN, 0, REST),
	write_bytes(FD, REST).
write_bytes(FD, DATA) :-
	foreign_call(raw_write(FD, DATA, N)),
	skip_bytes(N, DATA, REST),
	!, write_bytes(FD, REST).


%% will return [] on EOF, and block after that

read_bytes(FD, LEN, BYTES) :-
	foreign_call(raw_read(FD, LEN, BYTES)).


%% probably generally useful, find out how this is usually called
skip_bytes(0, _, []).
skip_bytes(N, [_|R], R2) :-
	N2 is N - 1,
	!, skip_bytes(N2, R, R2).


%% close file-descriptor

close_fd(FD) :-
	foreign_call(close_fd(FD)).


%% wait for I/O on file-descriptors

poll_fds(FDS, READY) :-
	poll_fds(FDS, -1, READY).

poll_fds(FDS, TIMEOUT, READY) :-
	foreign_call(poll_fds(FDS, TIMEOUT, READY)).
