%%% macro-expansion


% predefined macros

% NOTE: macros may not introduce new variables - these are scanned
%       before a toplevel expression is compiled.

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
macro(shell(CMD, STATUS), foreign_call(shell_command(CMD, STATUS))).
macro(atom_codes(A, LST), foreign_call(atom_codes(A, LST))).
macro(number_codes(N, LST), foreign_call(number_codes(N, LST))).
macro(functor(T, N, A), foreign_call(functor(T, N, A))).
macro(arg(I, T, X), foreign_call(term_arg(I, T, X))).
macro(current_input(S), foreign_call(current_input_stream(S))).
macro(current_output(S), foreign_call(current_output_stream(S))).


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
auto_include(select/3, 'lists.pl').

auto_include(compare/3, 'misc.pl').
auto_include(shell/1, 'misc.pl').

auto_include(tab/1, 'io.pl').
auto_include(skip/1, 'io.pl').
auto_include(get/1, 'io.pl').
auto_include(see/1, 'io.pl').
auto_include(seen/1, 'io.pl').
auto_include(seeing/1, 'io.pl').
auto_include(tell/1, 'io.pl').
auto_include(told/1, 'io.pl').
auto_include(telling/1, 'io.pl').

auto_include(op/3, 'op.pl').
auto_include(current_op/3, 'op.pl').

auto_include(throw/1, 'misc.pl').
auto_include(name/2, 'misc.pl').
auto_include(atomic_list_concat/2, 'misc.pl').
auto_include('=..'/2, 'misc.pl').
	     
auto_include(_, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).
