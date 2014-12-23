%%% miscellaneous predicates


compare(>, X, Y) :- X @> Y, !.
compare(<, X, Y) :- X @< Y, !.
compare(=, X, X).


%% this is just a fake, to have some sort of error-signalling operation

throw(EXN) :-
	foreign_call(current_error_stream(S)),
	foreign_call(set_current_output_stream(S)),
	display('\nERROR: '), display(EXN), nl,
	halt(70).

shell(CMD) :- shell(CMD, 0).

