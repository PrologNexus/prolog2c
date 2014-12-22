%%% list processing operations


member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

reverse(L, R) :- reverse(L, R, []).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

length([], 0).
length([_|X], N) :- length(X, N2), N is N2 + 1.
