%%% default prelude


nl :- foreign_call(write_char(10)).

display(X) :- foreign_call(basic_write(X)).
