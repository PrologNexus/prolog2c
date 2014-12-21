main :- a1.

a1 :- append([1,2,3],[4,5,6],X), display(X), nl, fail.
a1.

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).
