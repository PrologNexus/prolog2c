main :- a1.

a1 :- append([1,2,3],[4,5,6],X), display(X), nl, fail.
a1.

nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).
