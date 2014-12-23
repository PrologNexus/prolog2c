%%% basic I/O - Edinburgh'ish


see(user) :-
	foreign_call(set_current_input_stream(0)).
see(NAME) :-
	foreign_call(open_stream(NAME, 1, S)),
	foreign_call(set_current_input_stream(S)).

seeing(S) :- goreign_call(current_input_stream(S)).

%XXX doesn't handle case when stdin is closed
seen :-
	foreign_call(current_input_stream(S)),
	foreign_call(close_stream(S)),
	foreign_call(set_current_input_stream(0)).

tell(user) :-
	foreign_call(set_current_output_stream(0)).
tell(NAME) :-
	foreign_call(open_stream(NAME, 0, S)),
	foreign_call(set_current_output_stream(S)).

telling(S) :- foreign_call(current_output_stream(S)).

%XXX doesn't handle case when stdout is closed
told :-
	foreign_call(current_output_stream(S)),
	foreign_call(close_stream(S)),
	foreign_call(set_current_output_stream(0)).
