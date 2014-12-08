main :- (true -> foreign_call(write_char(65)); foreign_call(write_char(66))),
	(fail -> foreign_call(write_char(67)); foreign_call(write_char(68))),
	foo.

foo :- (bar -> foreign_call(write_char(48)); true), fail.
foo :- foreign_call(write_char(49)).

bar :- foreign_call(write_char(97)).
bar :- foreign_call(write_char(98)).
