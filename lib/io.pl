%%% basic I/O - Edinburgh'ish


see(user) :-
	foreign_call(set_current_input_stream(0)).
see(S) :-
	stream(S),
	!,
	foreign_call(set_current_input_stream(S)).	
see(NAME) :-
	foreign_call(open_stream(NAME, 1, 'rb', S)),
	foreign_call(set_current_input_stream(S)).

seen :-
	foreign_call(current_input_stream(S)),
	foreign_call(close_stream(S)).

tell(user) :-
	foreign_call(set_current_output_stream(0)).
tell(S) :-
	stream(S), 
	!,
	foreign_call(set_current_output_stream(S)).
tell(NAME) :-
	foreign_call(open_stream(NAME, 0, 'wb', S)),
	foreign_call(set_current_output_stream(S)).

told :-
	foreign_call(current_output_stream(S)),
	foreign_call(close_stream(S)).

append(NAME) :-
	foreign_call(open_stream(NAME, 0, 'ab', S)),
	foreign_call(set_current_output_stream(S)).

tab(N) :- N > 0, !, put(32), N2 is N - 1, tab(N2).
tab(_).

get(C) :-
	!, get0(C2),
	(C2 =:= -1, C = -1
	; C2 =\= 32, C = C2
	; get(C)).

skip(C) :-
	!, get0(C2),
	(C2 =:= -1
	; C == C2
	; skip(C)).

open(NAME, MODE, STREAM) :- open(NAME, MODE, STREAM, []).

open(NAME, write, STREAM, OPTIONS) :- open(NAME, MODE, 1, "w", OPTIONS, STREAM).
open(NAME, read, STREAM, OPTIONS) :- open(NAME, MODE, 0, "r", OPTIONS, STREAM).
open(NAME, append, STREAM, OPTIONS) :- open(NAME, MODE, 0, "a", OPTIONS, STREAM).

open(NAME, MODE, INPUT, MODE, [], STREAM) :-
	name(M, MODE),
	foreign_call(open_stream(NAME, INPUT, M, STREAM)).
open(NAME, MODE, INPUT, MODE, [type(text)|MORE], STREAM) :-
	open(NAME, MODE, INPUT, MODE, MORE, STREAM).
open(NAME, MODE, INPUT, MODE, [type(binary)|MORE], STREAM) :-
	append(MODE, "b", MODE2),
	open(NAME, MODE, INPUT, MODE2, MORE, STREAM).
open(NAME, MODE, INPUT, MODE, [_|MORE], STREAM) :-
	open(NAME, MODE, INPUT, MODE, MORE, STREAM).

read_string(LEN, ATM) :-
	foreign_call(read_string(LEN, A1)),
	(A1 == 0 -> foreign_call(retry_string_to_list(ATM)); ATM = A1).

read_line(ATM) :-
	foreign_call(read_line(A1)),
	(A1 == 0 -> foreign_call(retry_string_to_list(ATM)); ATM = A1).

set_input(user) :- foreign_call(set_current_input_stream(0)), !.
set_input(S) :- foreign_call(set_current_input_stream(S)).

set_output(user) :- foreign_call(set_current_output_stream(0)), !.
set_output(S) :- foreign_call(set_current_output_stream(S)).

set_error_output(user) :- foreign_call(set_current_error_stream(0)), !.
set_error_output(S) :- foreign_call(set_current_error_stream(S)).

flush_output :- foreign_call(current_output_stream(S)), foreign_call(flush_output(S)).
flush_output(user) :- flush_output.
flush_output(S) :- foreign_call(flush_output(S)).

at_end_of_stream :- foreign_call(current_output_stream(S)), foreign_call(at_eof(S)).
at_end_of_stream(user) :- at_end_of_stream.
at_end_of_stream(S) :- foreign_call(at_eof(S)).
