%%%% set-operations


union([], X, X).
union([X|R], Y, Z):- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]):- union(R, Y, Z).

intersection([], X, []).
intersection([X|R], Y, [X|Z]) :- member(X, Y), !, intersection(R, Y, Z).
intersection([X|R], Y, Z) :- intersection(R, Y, Z).

difference([], _, []) :- !.
difference([A|C], B, D) :- member(A, B), !, difference(C, B, D).
difference([A|B], C, [A|D]) :- difference(B, C, D).
