main :- foo.
main :- foreign_call(basic_write('ok.')), nl.

nl :- foreign_call(write_char(10)).

foo :- fail.
