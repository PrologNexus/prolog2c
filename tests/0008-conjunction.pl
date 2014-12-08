main :- a, b, c.

a :- true, foreign_call(write_char(97)).

b :- fail, foreign_call(write_char(98)).
b.

c :- foreign_call(write_char(99)), fail, foreign_call(write_char(100)).
c.
