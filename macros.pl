%%% macro-expansion


% predefined macros

macro(command_line_arguments(X), foreign_call(command_line_arguments(X))).
macro(display(X), foreign_call(basic_write(X))).
macro(exists_file(NAME), foreign_call(file_exists(NAME))).
macro(garbage_collect, foreign_call(gc)).
macro(halt(C), foreign_call(halt(C))).
macro(halt, foreign_call(halt(0))).
macro(nl, foreign_call(put_byte(10))).
macro(put(BYTE), foreign_call(put_byte(BYTE))).
macro(get0(BYTE), foreign_call(get_byte(BYTE))).
macro(peek(BYTE), foreign_call(peek_byte(BYTE))).


% nothing matches - tryi auto-include and finally, fail
macro(TERM, TERM) :-
	TERM =.. [NAME|_],
	atom(NAME),
	auto_include(NAME, FILE),
	add_boilerplate(FILE, (:- include(FILE))).


%% auto-include definitions

auto_include(_, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	display([TAG, CODE]), nl,
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).
