main :- foo(123).

foo(X) :- foreign_call(basic_write(X)).
