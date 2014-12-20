%%% default prelude


nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).

member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

halt(C) :- foreign_call(halt(C)).
halt :- foreign_call(halt(0)).

gc :- foreign_call(gc(0)).

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

command_line_arguments(X) :- foreign_call(command_line_arguments(X)).

reverse(L, R) :- reverse(L, R, []).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

union([], X, X).
union([X|R], Y, Z):- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]):- union(R, Y, Z).

intersection([], X, []).
intersection([X|R], Y, [X|Z]) :- member(X, Y), !, intersection(R, Y, Z).
intersection([X|R], Y, Z) :- intersection(R, Y, Z).

difference([], _, []) :- !.
difference([A|C], B, D) :- member(A, B), !, difference(C, B, D).
difference([A|B], C, [A|D]) :- difference(B, C, D).
