:- initialization (display(1), display('\n')).

display(X) :- foreign_call(basic_write(X)).
