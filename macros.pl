%%% macros and other information about builtins


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
macro(current_error_output(S), foreign_call(current_error_stream(S))).
macro(read(T), read1(T)).
macro(read(T, V), read1(T, V)).
macro(enable_trace(F), foreign_call(enable_trace(F))).
macro(getpid(PID), foreign_call(get_process_id(PID))).
macro(sleep(SECS), foreign_call(sleep_for_seconds(SECS))).
macro(set_random_seed(SEED), foreign_call(set_random_seed(SEED))).
macro(flush, foreign_call(flush_output)).


% nothing matches - tryi auto-include and finally, fail
macro(TERM, TERM) :-
	functor(TERM, NAME, ARITY),
	atom(NAME),
	auto_include(NAME/ARITY, FILE),
	add_boilerplate(FILE, (:- include(FILE))).


%% auto-include definitions

auto_include(length/2, 'lib/lists.pl').
auto_include(append/3, 'lib/lists.pl').
auto_include(member/2, 'lib/lists.pl').
auto_include(reverse/2, 'lib/lists.pl').
auto_include(memberchk/2, 'lib/lists.pl').

auto_include(compare/3, 'lib/misc.pl').
auto_include(shell/1, 'lib/misc.pl').
auto_include(between/3, 'lib/misc.pl').

auto_include(tab/1, 'lib/io.pl').
auto_include(skip/1, 'lib/io.pl').
auto_include(get/1, 'lib/io.pl').
auto_include(see/1, 'lib/io.pl').
auto_include(seen/0, 'lib/io.pl').
auto_include(seeing/1, 'lib/io.pl').
auto_include(tell/1, 'lib/io.pl').
auto_include(told/0, 'lib/io.pl').
auto_include(telling/1, 'lib/io.pl').

auto_include(op/3, 'lib/op.pl').
auto_include(current_op/3, 'lib/op.pl').

auto_include(throw/1, 'lib/misc.pl').
auto_include(name/2, 'lib/misc.pl').
auto_include('=..'/2, 'lib/misc.pl').
auto_include(deref_term/3, 'lib/misc.pl').
auto_include(copy_term/2, 'lib/misc.pl').

auto_include(union/3, 'lib/sets.pl').
auto_include(intersection/3, 'lib/sets.pl').
auto_include(subtract/3, 'lib/sets.pl').
auto_include(select/3, 'lib/sets.pl').
auto_include(symdiff/3, 'lib/sets.pl').

auto_include(write/1, 'lib/write.pl').
auto_include(writeq/1, 'lib/write.pl').

auto_include(recorda/2, 'lib/rdb.pl').
auto_include(recorda/3, 'lib/rdb.pl').
auto_include(recordz/2, 'lib/rdb.pl').
auto_include(recordz/3, 'lib/rdb.pl').
auto_include(recorded/2, 'lib/rdb.pl').
auto_include(recorded/3, 'lib/rdb.pl').

auto_include('$findall_start'/0, 'lib/findall.pl').
auto_include('$findall_push'/1, 'lib/findall.pl').
auto_include('$findall_collect'/1, 'lib/findall.pl').

auto_include(sort/2, 'lib/sorts.pl').
auto_include(keysort/2, 'lib/sorts.pl').
auto_include(merge/3, 'lib/sorts.pl').

auto_include(list_to_ord_set/2, 'lib/ordset.pl').
auto_include(ord_disjoint/2, 'lib/ordset.pl').
auto_include(ord_insert/3, 'lib/ordset.pl').
auto_include(ord_intersect/2, 'lib/ordset.pl').
auto_include(ord_intersect/3, 'lib/ordset.pl').
auto_include(ord_seteq/2, 'lib/ordset.pl').
auto_include(ord_subset/2, 'lib/ordset.pl').
auto_include(ord_symdiff/2, 'lib/ordset.pl').
auto_include(ord_union/2, 'lib/ordset.pl').
auto_include(ord_subtract/2, 'lib/ordset.pl').
auto_include(ord_memberchk/2, 'lib/ordset.pl').

auto_include(read_tokens/2, 'lib/rdtok.pl').
auto_include(read1/1, 'lib/read.pl').

auto_include(clause/2, 'lib/cdb.pl').
auto_include(clause/3, 'lib/cdb.pl').
auto_include(retract/1, 'lib/cdb.pl').
auto_include(abolish/1, 'lib/cdb.pl').
auto_include(asserta/1, 'lib/cdb.pl').
auto_include(asserta/2, 'lib/cdb.pl').
auto_include(assertz/1, 'lib/cdb.pl').
auto_include(assertz/2, 'lib/cdb.pl').

auto_include(_, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).
