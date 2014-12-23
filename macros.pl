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
macro(erase(REF), foreign_call(db_erase(REF))).
macro(getenv(NAME, VAL), foreign_call(get_environment_variable(NAME, VAL))).
macro(shell(CMD), foreign_call(shell_command(CMD, 0))).
macro(shell(CMD, STATUS), foreign_call(shell_command(CMD, STATUS))).


% nothing matches - tryi auto-include and finally, fail
macro(TERM, TERM) :-
	functor(TERM, NAME, ARITY),
	atom(NAME),
	auto_include(NAME/ARITY, FILE),
	add_boilerplate(FILE, (:- include(FILE))).


%% auto-include definitions

auto_include(length/2, 'lists.pl').
auto_include(append/3, 'lists.pl').
auto_include(member/2, 'lists.pl').
auto_include(reverse/2, 'lists.pl').

auto_include(compare/3, 'misc.pl').

auto_include(_, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).
