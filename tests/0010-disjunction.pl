main :- foo, bar, baz.

foo :- fail; foreign_call(basic_write('1')).
bar :- (twice, foreign_call(basic_write('2'))
       ; fail).
bar.

twice :- foreign_call(basic_write('3')).
twice :- foreign_call(basic_write('4')).

baz :- true; foreign_call(basic_write('5')).

