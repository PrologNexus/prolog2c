main :- foo.
main :- foreign_call(write_char(67)).

foo :- (!, fail; foreign_call(write_char(65))).
foo :- foreign_call(write_char(66)).
