%%% basic I/O - Edinburgh'ish


see(ALIAS) :-
	(ALIAS == user; ALIAS == user_input; ALIAS = current_input),
	!, foreign_call(set_current_input_stream(user_input)).
see(S) :-
	is_stream(S),
	!,
	foreign_call(set_current_input_stream(S)).	
see(NAME) :-
	foreign_call(open_stream(NAME, 1, 'rb', S)),
	foreign_call(set_current_input_stream(S)).

seen :-
	foreign_call(current_input_stream(S)),
	foreign_call(close_stream(S)).

tell(user) :-
	!, foreign_call(set_current_output_stream(user_output)).
tell(ALIAS) :-
	(ALIAS == user_output; ALIAS == user_error; ALIAS == current_output),
	!, foreign_call(set_current_output_stream(ALIAS)).
tell(S) :-
	is_stream(S), 
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

tab(N) :- tab(current_output, N).

tab(S, N) :- N > 0, !, put(S, 32), N2 is N - 1, tab(S, N2).
tab(_, _).

get(C) :- get(current_input, C).

get(S, C) :-
	!, get0(S, C2),
	(C2 =:= -1, C = -1
	; C2 =\= 32, C = C2
	; get(S, C)).

skip(C) :- skip(current_input, C).

skip(S, C) :-
	!, get0(S, C2),
	(C2 =:= -1
	; C == C2
	; skip(S, C)).

open(NAME, MODE, STREAM) :- open(NAME, MODE, STREAM, []).

open(NAME, write, STREAM, OPTIONS) :- open(NAME, 0, "w", OPTIONS, STREAM).
open(NAME, read, STREAM, OPTIONS) :- open(NAME, 1, "r", OPTIONS, STREAM).
open(NAME, append, STREAM, OPTIONS) :- open(NAME, 0, "a", OPTIONS, STREAM).

open(NAME, INPUT, MODE, [], STREAM) :-
	name(M, MODE),
	foreign_call(open_stream(NAME, INPUT, M, STREAM)).
open(NAME, INPUT, MODE, [type(text)|MORE], STREAM) :-
	open(NAME, INPUT, MODE, MORE, STREAM).
open(NAME, INPUT, MODE, [type(binary)|MORE], STREAM) :-
	append(MODE, "b", MODE2),
	open(NAME, INPUT, MODE2, MORE, STREAM).
open(NAME, INPUT, MODE, [_|MORE], STREAM) :-
	open(NAME, INPUT, MODE, MORE, STREAM).

read_string(STREAM, LEN, ATM) :-
	foreign_call(read_string(STREAM, LEN, A1)),
	(A1 == 0 -> foreign_call(retry_string_to_list(ATM)); ATM = A1).

read_string(LEN, ATM) :- read_string(current_input, LEN, ATM).

read_line(STREAM, ATM) :-
	foreign_call(read_line(STREAM, A1)),
	(A1 == 0 -> foreign_call(retry_string_to_list(ATM)); ATM = A1).

read_line(ATM) :- read_line(current_input, ATM).

set_input(S) :- foreign_call(set_current_input_stream(S)).
set_output(S) :- foreign_call(set_current_output_stream(S)).

flush_output :- foreign_call(current_output_stream(S)), foreign_call(flush_output(S)).
flush_output(S) :- foreign_call(flush_output(S)).

at_end_of_stream :- foreign_call(current_output_stream(S)), foreign_call(at_eof(S)).
at_end_of_stream(S) :- foreign_call(at_eof(S)).

set_stream_position(S, P) :- foreign_call(set_stream_position(S, P)).

'$stream_property'(position(P), S) :- foreign_call(stream_position(S, P)).
'$stream_property'(tty(B), S) :- (foreign_call(tty_stream(S)) -> B = true; B = false).
