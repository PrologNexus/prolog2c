%%% macro-expansion


% predefined macros

macro(display(X), foreign_call(basic_write(X))).
macro(garbage_collect, foreign_call(gc)).
macro(nl, foreign_call(write_char(10))).


% nothing matches - default is to fail
macro(_, _) :- fail.
