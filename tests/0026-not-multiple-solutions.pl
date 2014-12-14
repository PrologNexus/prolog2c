main :- \+((member(X,[1,2]), display(X), nl)), fail.
main.

member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).

