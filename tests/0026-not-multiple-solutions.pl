main :- \+((member(X,[1,2]), display(X), nl)), fail.
main.

member(X, [X|_]).
member(X, [_|R]) :- member(X, R).
