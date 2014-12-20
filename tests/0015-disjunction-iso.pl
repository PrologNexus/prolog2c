% ISO-example to ";" operator

nl :- foreign_call(write_char(10)).
display(X) :- foreign_call(basic_write(X)).

main :-
	((insect(X), fly(X)); (has_legs(X, 6), fly(X))), display(X), nl, fail.
main.

insect(bee).
insect(ant).

fly(bee).

has_legs(X, 6) :- insect(X).
