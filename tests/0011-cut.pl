main :- foo.
main :- foreign_call(write_char(66)).

foo :-
	bar,
	!,
	fail.
foo :- foreign_call(write_char(65)).

bar :- foreign_call(write_char(48)).
bar :- foreign_call(write_char(49)).
