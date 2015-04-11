%%% generic tokenizer


%% state: s(FILENAME, LINE, [STREAM, STATE]|[])
%
% use FILENAME == [] to tokenize input string only


%% expects:
%
% tok_pp_line(STR, STATE1, STATE, STR2)
% tok_ws(STATE1, STATE, STR1, STR)
% tok_id(ATOM, STR1, STR)
% tok_num(NUMBER, STR1, STR)
% tok_op(ATOM, STR1, STR)
% tok_special(ATOM, STR1, STR)
% tok_str(STATE1, STATE, LST, STR1, STR)
% tok_fail(STATE, C)


%% tokens:
%
% op(OPNAME, LOC)
% str(STRING, LOC)
% id(NAME, LOC)
% num(NUMBER, LOC)
% special(NAME, LOC)
%
% where LOC = FILENAME/LINE


next_tokens(COUNT, TOKS, S1, STR1, S, STR) :-
	tokstr(COUNT, S1, S, TOKS, STR1, STR).

tokstr(0, S, S, []) --> [], !.
tokstr(N, S1, S, TOKS) -->
	eoi, {getline(S1, S2, LINE)}, !, tokline(N, S2, S, LINE, TOKS).
tokstr(N, S1, S, TOKS) -->
	tok_ws(S1, S2), !, tokstr(N, S2, S, TOKS).
tokstr(N, S1, S, [T|OKS]) -->
	tok_tok(S1, S2, T), !, {N2 is N - 1}, tokstr(N2, S2, S, OKS).

tok_tok(S, S, special(N, LOC)) --> tok_special(N), {getloc(S, LOC)}.
tok_tok(S1, S, str(LST, LOC)) --> tok_str(S1, S, LST), {getloc(S1, LOC)}.
tok_tok(S, S, op(N, LOC)) --> tok_op(N), {getloc(S, LOC)}.
tok_tok(S, S, id(N, LOC)) --> tok_id(N), {getloc(S, LOC)}.
tok_tok(S, S, num(N, LOC)) --> tok_num(N), {getloc(S, LOC)}.
tok_tok(S, _, _) --> [C], tok_fail(S, C), !, {fail}.

getloc(s(FN, LN, _), FN/LN).

tokline(N, S1, S, end_of_file, TOKS) --> !, nextstate(N, S1, S, TOKS).
tokline(N, S1, S, STR, TOKS, _, OUT) :-
	tok_pp_line(STR, S1, S2, STR2),
	!, tokstr(N, S2, S, TOKS, STR2, OUT).

getline(s([], LN, SS), s([], LN, SS), end_of_file) :- !.
getline(s(FN, LN, SS), s(FN, LN2, SS), LINE) :- LN2 is LN + 1, read_line(LINE).

nextstate(_, s(_, _, []), [], []) --> !.
nextstate(N, s(_, _, [INPUT, S1]), S, TOKS, _, STR) :-
	seen, see(INPUT),
	!, tokstr(N, S1, S, TOKS, [], STR).

openfile(FNAME, S1, s(FNAME, 0, [INPUT, S1])) :- seeing(INPUT), see(FNAME).
       
eoi([], []).


%% skip single- or multiline comment

skip_line(S, S, _, []).	

skip_comment(CMT, S, S) --> match1(CMT), !.
skip_comment(CMT, S1, S) --> [_], !, skip_comment(CMT, S1, S).
skip_comment(CMT, S1, S, [], STR) :-
	getline(S1, S2, LINE),
	(LINE \== end_of_file; throw(tokenizer_error('unexpected end of comment', S2))),
	atom_codes(LINE, STR1),
	!, skip_comment(CMT, S2, S, STR1, STR).


%% find token in table

find_token([OP|OPT], IN, OP, OUT) :- match1(OP, IN, OUT), !.
find_token([_|OPT], IN, OP, OUT) :- find_token(OPT, IN, OP, OUT).


%% match string in input

match1([], IN, IN).
match1([C|R], [C|R2], OUT) :- match1(R, R2, OUT).


%% scan delimited string

scan_delim(DEL, S, S, []) --> match1(DEL).
scan_delim(DEL, S1, S, [C|R]) --> [C], scan_delim(DEL, S1, S, R).
scan_delim(DEL, S1, _, _) -->
	{throw(tokenizer_error('unexpected end of delimited string', S1))}.


%% collect characters from a particular class

idchars([C|R]) --> [C], {alpha(C); digit(C); C == 95}, !, idchars(R).
idchars([]) --> [].

numchars(T, [C|R]) --> [C], {digit(C)}, !, numchars(T, R).
numchars(int, [46|R]) --> ".", !, numchars(float, R).
numchars(_, []) --> [].


%% character classifiers

alpha(C) :- uppercase(C); lowercase(C).
digit(C) :- C >= 48, C =< 57.
uppercase(C) :- C >= 65, C =< 90.
lowercase(C) :- C >= 97, C =< 122.


/* example:

%% example implementations

tok_pp_line(STR, S, S, STR).

tok_ws(S, S) --> [C], {memberchk(C, " \t")}, !.
tok_ws(S1, S) -->
        "(*", skip_comment("*)", S1, S2), !,
        tok_ws_k(S2, S).

tok_ws_k(S, S) --> [].
tok_ws_k(S1, S) --> tok_ws(S1, S).

tok_id(N) --> [C], {alpha(C); C == 95}, idchars(LST), {atom_codes(N, [C|LST])}.

tok_num(N) --> [C], {digit(C)}, numchars(int, LST), {number_codes(N, [C|LST])}.

tok_special(N) --> [C], {memberchk(C, "()[]{},;:"), atom_codes(N, [C])}.

tok_op(N, IN, OUT) :- optable(OPT), find_token(OPT, IN, OP, OUT), atom_codes(N, OP).

tok_str(S1, S, STR) --> "\"", scan_delim("\"", S1, S, STR).

tok_fail(S, C) --> throw(syntax_error(S, C)).

optable(["+", "-", "*", "/", "."]).


%%

main :-
	tokstr(10, s(here,0,[]), S, TOKS, "", STR), writeq(TOKS),nl,
	tokstr(100, S, _, TOKS2, STR, _), writeq(TOKS2),nl.

*/
