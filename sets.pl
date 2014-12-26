%%%% set-operations


union([], X, X).
union([X|R], Y, Z):- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]):- union(R, Y, Z).

intersection([], X, []).
intersection([X|R], Y, [X|Z]) :- member(X, Y), !, intersection(R, Y, Z).
intersection([X|R], Y, Z) :- intersection(R, Y, Z).

subtract([], _, []) :- !.
subtract([A|C], B, D) :- member(A, B), !, subtract(C, B, D).
subtract([A|B], C, [A|D]) :- subtract(B, C, D).

select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
	select(Elem, Tail, Rest).
