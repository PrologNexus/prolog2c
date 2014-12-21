%%% misc support library


gen(T) :- write(T).
gen(T1, T2) :- gen(T1), gen(T2).
gen(T1, T2, T3) :- gen(T1, T2), gen(T3).
gen(T1, T2, T3, T4) :- gen(T1, T2, T3), gen(T4).
gen(T1, T2, T3, T4, T5) :- gen(T1, T2, T3, T4), gen(T5).

error(MSG) :-
	tell(user),
	write('ERROR: '),
	forall(member(X, MSG), write(X)), nl,
	halt(1).

map(_, [], []) :- !.
map(G, [X|Y], [X2|Y2]) :-
	call(G, X, X2), !, map(G, Y, Y2).

iota(N, L) :- iota(0, N, L).
iota(N, N, []).
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

file_name_string(IFILE, F) :- append(F, [46|_], IFILE), !.
file_name_string(IFILE, IFILE).

mangle_name(NAME, MNAME) :-
	name(NAME, STRING),
	map(mangle_char, STRING, MSTRING),
	concatenate(["___"|MSTRING], MSTRING2),
	name(MNAME, MSTRING2).

mangle_char(C, [C]) :- (C >= 97, C =< 122; C >= 48, C =< 57), !.
mangle_char(C1, [95, C2, C3]) :-
	N is C1 // 16, hexdigit(N, C2),
	M is C1 - N * 16, hexdigit(M, C3).

hexdigit(N, M) :- N < 10, M is N + 48, !.
hexdigit(N, M) :- M is N + 87.

concatenate([], []).
concatenate([X|Y], Z) :-
	concatenate(Y, Z2),
	append(X, Z2, Z).

locate_file(NAME, REALNAME) :-
	recorded(include_path, PATH),
	locate_file(NAME, PATH, REALNAME).

locate_file(NAME, [], _) :-
	error(['include-file not found: ', NAME]).
locate_file(NAME, [DIR|_], REALNAME) :-
	atomic_list_concat([DIR, '/', NAME], REALNAME),
	file_exists(REALNAME).
locate_file(NAME, [DIR|_], REALNAME) :-
	atomic_list_concat([DIR, '/', NAME, '.pl'], REALNAME),
	file_exists(REALNAME).
locate_file(NAME, [_|MORE], REALNAME) :-
	locate_file(NAME, MORE; REALNAME).
