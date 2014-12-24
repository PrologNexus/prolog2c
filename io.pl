%%% basic I/O - Edinburgh'ish


see(user) :-
	foreign_call(set_current_input_stream(0)).
see(S) :-
	stream(S),
	!,
	foreign_call(set_current_input_stream(S)).	
see(NAME) :-
	foreign_call(open_stream(NAME, 1, S)),
	foreign_call(set_current_input_stream(S)).

seeing(S) :- foreign_call(current_input_stream(S)).

seen :-
	foreign_call(current_input_stream(S)),
	foreign_call(close_stream(S)),
	foreign_call(set_current_input_stream(0)).

tell(user) :-
	foreign_call(set_current_output_stream(0)).
tell(S) :-
	stream(S), 
	!,
	foreign_call(set_current_output_stream(S)).
tell(NAME) :-
	foreign_call(open_stream(NAME, 0, S)),
	foreign_call(set_current_output_stream(S)).

telling(S) :- foreign_call(current_output_stream(S)).

told :-
	foreign_call(current_output_stream(S)),
	foreign_call(close_stream(S)),
	foreign_call(set_current_output_stream(0)).

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
