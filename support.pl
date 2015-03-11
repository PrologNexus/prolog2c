%%% misc support library


gen(T) :- display(T).
gen(T1, T2) :- gen(T1), gen(T2).
gen(T1, T2, T3) :- gen(T1, T2), gen(T3).
gen(T1, T2, T3, T4) :- gen(T1, T2, T3), gen(T4).
gen(T1, T2, T3, T4, T5) :- gen(T1, T2, T3, T4), gen(T5).

gen_list([]).
gen_list([X|R]) :- gen(X), !, gen_list(R).

error(MSG) :-
	current_error_output(ERR), tell(ERR),
	display('ERROR: '),
	forall(member(X, MSG), write(X)), nl, nl,
	halt(1).

message(MSG) :-
	(recorded(silent, yes)
	; forall(member(X, MSG), write(X)), nl
	).

iota(N, L) :- iota(0, N, L).
iota(N, N, []) :- !.
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

file_name_string(IFILE, F) :- append(F, [46|_], IFILE), !.
file_name_string(IFILE, IFILE).

mangle_name(NAME, MNAME) :-
	name(NAME, STRING),
	findall(CS, (member(C, STRING), mangle_char(C, CS)), MSTRING),
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

%% extract second arg of terms in list - this doesn't use findall/3,
%% to avoid renaming
map_second([], []).
map_second([T|MORE], [V|REST]) :-
	arg(2, T, V),
	map_second(MORE, REST).


%%% locating files

locate_file(NAME, RNAME) :-
	recorded(include_path, PATH),
	locate_file(NAME, PATH, RNAME).
locate_file(NAME, _) :-
	error(['include-file not found: ', NAME]).

locate_file(_, [], _) :- !, fail.
locate_file(NAME, [DIR|_], REALNAME) :-
	atom_concat(DIR, '/', DIR1), '$locate_file'(DIR1, NAME, REALNAME), !.
locate_file(NAME, [_|MORE], REALNAME) :- locate_file(NAME, MORE, REALNAME).

'$locate_file'(D, N, R) :- atom_concat(D, N, N1), '$locate_file_2'(N1, R).

'$locate_file_2'(N, R) :- atom_concat(N, '.pl', R), exists_file(R).
'$locate_file_2'(N, N) :- exists_file(N).
