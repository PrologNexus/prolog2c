main :- foo, bar.

nl :- foreign_call(write_char(10)).

foo :-
	foreign_call(basic_write('foo1')), nl.
foo :-
	foreign_call(basic_write('foo2')), nl.

bar :-
	foreign_call(basic_write('bar1')), nl,
	fail.
bar :-
	foreign_call(basic_write('bar2')), nl,
	fail.
