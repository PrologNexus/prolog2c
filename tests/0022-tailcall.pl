
display(X) :- foreign_call(basic_write(X)).
nl :- foreign_call(write_char(10)).

main :- loop(10).

loop(0).
loop(X) :-
	display(X), nl,
	Y is X - 1, !, loop(Y).
