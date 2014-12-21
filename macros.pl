%%% macro-expansion


% predefined macros

macro(display(X), foreign_call(basic_write(X))).
macro(nl, foreign_call(write_char(10))).


% nothing matches - default is to fail
macro(_, _) :- fail.
