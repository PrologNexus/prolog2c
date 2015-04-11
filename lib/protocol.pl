%%% parse binary protocol


:- ensure_loaded(library(bits)).


%% parse_binary_block(FORMAT, STRING, DATA)
%% parse_binary_block(FORMAT, STRING, REMBITS, REMSTRING, DATA)
%
% where
%
%   FORMAT = [FIELDSPEC, ...]
%   FIELDSPEC = field(BITCOUNT, NAME)
%
%   DATA = [FIELD, ...]
%   FIELD = NAME-VALUE

parse_binary_block(FORMAT, STRING, DATA) :-
	parse_binary_block(FORMAT, STRING, [], [], DATA).

parse_binary_block(FORMAT, STRING, REST, STRING2, DATA) :-
	parse_binary_block(FORMAT, [], STRING, REST, STRING2, DATA).
	    
parse_binary_block([], R, S, R, S, []).
parse_binary_block([field(BITS, NAME)|FORMAT], REST, STRING, REST2, STRING2, [NAME-VAL|DATA]) :-
	length(BITVALS, BITS),
	scan_field(BITVALS, NAME, REST, STRING, REST3, STRING3),
	number_bits(VAL, BITVALS),
	!,
	parse_binary_block(FORMAT, REST3, STRING3, REST2, STRING2, DATA).

scan_field([], _, R, S, R, S) :- !.
scan_field([BIT|BITS], NAME, [], [CODE|S], R2, S2) :-
	code_bits(CODE, CBITS),
	!,
	scan_field([BIT|BITS], NAME, CBITS, S, R2, S2).
scan_field([BIT|BITS], NAME, [BIT|REST], S, R2, S2) :-
	!,
	scan_field(BITS, NAME, REST, S, R2, S2).


/*

format([field(3, a), field(4, b), field(16, c), field(1, d)]).

main :-
	format(F),
	parse_binary_block(F, "0123", R, S, D),
	writeq(D), nl,
	writeq(R), nl,
	writeq(S), nl.

*/
