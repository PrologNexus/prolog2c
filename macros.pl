%%% macros and other information about builtins


% predefined macros

% NOTE: macros may not introduce new variables - these are scanned
%       before a toplevel expression is compiled.

macro(command_line_arguments(X), foreign_call(command_line_arguments(X))).
macro(display(X), foreign_call(basic_write(0, X))).
macro(display(S, X), foreign_call(basic_write(S, X))).
macro(exists_file(NAME), foreign_call(file_exists(NAME))).
macro(exists_directory(NAME), foreign_call(dir_exists(NAME))).
macro(garbage_collect, foreign_call(gc)).
macro(halt(C), foreign_call(halt(C))).
macro(halt, foreign_call(halt(0))).
macro(nl, foreign_call(put_byte(0, 10))).
macro(nl(S), foreign_call(put_byte(S, 10))).
macro(put(BYTE), foreign_call(put_byte(0, BYTE))).
macro(put_byte(BYTE), foreign_call(put_byte(0, BYTE))).
macro(put_byte(S, BYTE), foreign_call(put_byte(S, BYTE))).
macro(put_code(BYTE), foreign_call(put_byte(0, BYTE))).
macro(put_code(S, BYTE), foreign_call(put_byte(S, BYTE))).
macro(get0(BYTE), foreign_call(get_byte(0, BYTE))).
macro(get_byte(BYTE), foreign_call(get_byte(0, BYTE))).
macro(get_byte(S, BYTE), foreign_call(get_byte(S, BYTE))).
macro(get_code(BYTE), foreign_call(get_byte(0, BYTE))).
macro(get_code(S, BYTE), foreign_call(get_byte(S, BYTE))).
macro(peek(BYTE), foreign_call(peek_byte(0, BYTE))).
macro(peek_byte(BYTE), foreign_call(peek_byte(0, BYTE))).
macro(peek_byte(S, BYTE), foreign_call(peek_byte(S, BYTE))).
macro(peek_code(BYTE), foreign_call(peek_byte(0, BYTE))).
macro(peek_code(S, BYTE), foreign_call(peek_byte(S, BYTE))).
macro(erase(REF), foreign_call(db_erase(REF))).
macro(getenv(NAME, VAL), foreign_call(get_environment_variable(NAME, VAL))).
macro(shell(CMD, STATUS), foreign_call(shell_command(CMD, STATUS))).
macro(functor(T, N, A), foreign_call(functor(T, N, A))).
macro(arg(I, T, X), foreign_call(term_arg(I, T, X))).
macro(seeing(S), foreign_call(current_input_stream(S))).
macro(telling(S), foreign_call(current_output_stream(S))).
macro(current_input(S), foreign_call(current_input_stream(S))).
macro(current_output(S), foreign_call(current_output_stream(S))).
macro(current_error_output(S), foreign_call(current_error_stream(S))).
macro(read(T), read1(T)).
macro(read(S, T), read2(S, T)).
macro(enable_trace(F), foreign_call(enable_trace(F))).
macro(getpid(PID), foreign_call(get_process_id(PID))).
macro(sleep(SECS), foreign_call(sleep_for_seconds(SECS))).
macro(set_random_seed(SEED), foreign_call(set_random_seed(SEED))).
macro(atom_hash(ATOM, HASH), foreign_call(atom_hash(ATOM, HASH))).
macro(acyclic_term(X), foreign_call(acyclic_term(X))).
macro(close(S), foreign_call(close_stream(S))).
macro(atom_length(A, L), foreign_call(atom_length(A, L))).
macro(ground(X), foreign_call(ground(X))).
macro(rename_file(X, Y), foreign_call(rename_file(X, Y))).
macro(delete_file(X), foreign_call(delete_file(X))).


% nothing matches - tryi auto-include and finally, fail
macro(TERM, TERM) :-
	functor(TERM, NAME, ARITY),
	atom(NAME),
	auto_include(NAME, ARITY, FILE),
	add_boilerplate(FILE, (:- include(FILE))).


%% auto-include definitions

auto_include(length, 2, 'lib/lists.pl').
auto_include(append, 3, 'lib/lists.pl').
auto_include(member, 2, 'lib/lists.pl').
auto_include(reverse, 2, 'lib/lists.pl').
auto_include(memberchk, 2, 'lib/lists.pl').

auto_include(compare, 3, 'lib/misc.pl').
auto_include(shell, 1, 'lib/misc.pl').
auto_include(between, 3, 'lib/misc.pl').
auto_include(atom_codes, 2, 'lib/misc.pl').
auto_include(number_codes, 2, 'lib/misc.pl').
auto_include(char_code, 2, 'lib/misc.pl').

auto_include(tab, 1, 'lib/io.pl').
auto_include(skip, 1, 'lib/io.pl').
auto_include(get, 1, 'lib/io.pl').
auto_include(see, 1, 'lib/io.pl').
auto_include(seen, 0, 'lib/io.pl').
auto_include(seeing, 1, 'lib/io.pl').
auto_include(tell, 1, 'lib/io.pl').
auto_include(told, 0, 'lib/io.pl').
auto_include(telling, 1, 'lib/io.pl').
auto_include(open, 3, 'lib/io.pl').
auto_include(open, 4, 'lib/io.pl').
auto_include(read_string, 2, 'lib/io.pl').
auto_include(read_line, 1, 'lib/io.pl').
auto_include(flush_output, 0, 'lib/io.pl').
auto_include(flush_output, 1, 'lib/io.pl').
auto_include(at_end_of_stream, 0, 'lib/io.pl').
auto_include(at_end_of_stream, 1, 'lib/io.pl').
auto_include(set_input, 0, 'lib/io.pl').
auto_include(set_output, 1, 'lib/io.pl').
auto_include(set_error_output, 1, 'lib/io.pl').
	     
auto_include(op, 3, 'lib/op.pl').
auto_include(current_op, 3, 'lib/op.pl').

auto_include(throw, 1, 'lib/misc.pl').
auto_include(name, 2, 'lib/misc.pl').
auto_include('=..', 2, 'lib/misc.pl').
auto_include(deref_term, 4, 'lib/misc.pl').
auto_include(copy_term, 2, 'lib/misc.pl').
auto_include(duplicate_term, 2, 'lib/misc.pl').
auto_include(unify_with_occurs_check, 2, 'lib/misc.pl').

auto_include(union, 3, 'lib/sets.pl').
auto_include(intersection, 3, 'lib/sets.pl').
auto_include(subtract, 3, 'lib/sets.pl').
auto_include(select, 3, 'lib/sets.pl').
auto_include(symdiff, 3, 'lib/sets.pl').

auto_include(write, 1, 'lib/write.pl').
auto_include(writeq, 1, 'lib/write.pl').
auto_include(write, 2, 'lib/write.pl').
auto_include(writeq, 2, 'lib/write.pl').

auto_include(recorda, 2, 'lib/rdb.pl').
auto_include(recorda, 3, 'lib/rdb.pl').
auto_include(recordz, 2, 'lib/rdb.pl').
auto_include(recordz, 3, 'lib/rdb.pl').
auto_include(recorded, 2, 'lib/rdb.pl').
auto_include(recorded, 3, 'lib/rdb.pl').

auto_include('$findall_start', 0, 'lib/findall.pl').
auto_include('$findall_push', 1, 'lib/findall.pl').
auto_include('$findall_collect', 1, 'lib/findall.pl').
auto_include('$bagof_start', 3, 'lib/findall.pl').
auto_include('$bagof_finish', 1, 'lib/findall.pl').

auto_include(sort, 2, 'lib/sorts.pl').
auto_include(keysort, 2, 'lib/sorts.pl').
auto_include(merge, 3, 'lib/sorts.pl').

auto_include(list_to_ord_set, 2, 'lib/ordset.pl').
auto_include(ord_disjoint, 2, 'lib/ordset.pl').
auto_include(ord_insert, 3, 'lib/ordset.pl').
auto_include(ord_intersect, 2, 'lib/ordset.pl').
auto_include(ord_intersect, 3, 'lib/ordset.pl').
auto_include(ord_seteq, 2, 'lib/ordset.pl').
auto_include(ord_subset, 2, 'lib/ordset.pl').
auto_include(ord_symdiff, 2, 'lib/ordset.pl').
auto_include(ord_union, 2, 'lib/ordset.pl').
auto_include(ord_subtract, 2, 'lib/ordset.pl').
auto_include(ord_memberchk, 2, 'lib/ordset.pl').

auto_include(read_tokens, 2, 'lib/rdtok.pl').
auto_include(read1, 1, 'lib/read.pl').
auto_include(read2, 1, 'lib/read.pl').

auto_include(clause, 2, 'lib/cdb.pl').
auto_include(clause, 3, 'lib/cdb.pl').
auto_include(retract, 1, 'lib/cdb.pl').
auto_include(abolish, 1, 'lib/cdb.pl').
auto_include(asserta, 1, 'lib/cdb.pl').
auto_include(asserta, 2, 'lib/cdb.pl').
auto_include(assertz, 1, 'lib/cdb.pl').
auto_include(assertz, 2, 'lib/cdb.pl').

auto_include(succ, 2, 'lib/arith.pl').
auto_include(plus, 3, 'lib/arith.pl').
auto_include(times, 3, 'lib/arith.pl').
auto_include(divide, 4, 'lib/arith.pl').

auto_include(writef, 1, 'lib/writef.pl').
auto_include(writef, 2, 'lib/writef.pl').
auto_include(fwritef, 2, 'lib/writef.pl').
auto_include(fwritef, 3, 'lib/writef.pl').

auto_include('$delay_goal', 4, 'lib/co.pl').
auto_include('$freeze_goal', 4, 'lib/co.pl').
auto_include(dif, 2, 'lib/co.pl').

auto_include(current_prolog_flag, 2, 'lib/flags.pl').

auto_include(atom_concat, 3, 'lib/iso.pl').
auto_include(atom_chars, 2, 'lib/iso.pl').
auto_include(number_chars, 2, 'lib/iso.pl').
auto_include(get_char, 1, 'lib/iso.pl').
auto_include(get_char, 2, 'lib/iso.pl').
auto_include(peek_char, 1, 'lib/iso.pl').
auto_include(peek_char, 2, 'lib/iso.pl').
auto_include(put_char, 1, 'lib/iso.pl').
auto_include(put_char, 2, 'lib/iso.pl').

auto_include(_, _, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).


%% determinateness information

determinate_builtin(memberchk, 2).
determinate_builtin(compare, 3).
determinate_builtin(shell, 1).
determinate_builtin(tab, 1).
determinate_builtin(skip, 1).
determinate_builtin(get, 1).
determinate_builtin(see, 1).
determinate_builtin(seen, 0).
determinate_builtin(seeing, 1).
determinate_builtin(tell, 1).
determinate_builtin(told, 0).
determinate_builtin(telling, 1).
determinate_builtin(op, 3).
determinate_builtin(throw, 1).
determinate_builtin(name, 2).
determinate_builtin('=..', 2).
determinate_builtin(deref_term, 3).
determinate_builtin(copy_term, 2).
determinate_builtin(duplicate_term, 2).
determinate_builtin(write, 1).
determinate_builtin(writeq, 1).
determinate_builtin(write, 2).
determinate_builtin(writeq, 2).
determinate_builtin(recorda, 2).
determinate_builtin(recorda, 3).
determinate_builtin(recordz, 2).
determinate_builtin(recordz, 3).
determinate_builtin('$findall_start', 0).
determinate_builtin('$findall_push', 1).
determinate_builtin('$findall_collect', 1).
determinate_builtin(ord_memberchk, 2).
determinate_builtin(read_tokens, 2).
determinate_builtin(read1, 1).
determinate_builtin(read1, 2).
determinate_builtin(read2, 1).
determinate_builtin(abolish, 1).
determinate_builtin(asserta, 1).
determinate_builtin(asserta, 2).
determinate_builtin(assertz, 1).
determinate_builtin(assertz, 2).
determinate_builtin(open, 3).
determinate_builtin(open, 4).
determinate_builtin(succ, 2).
determinate_builtin(plus, 2).
determinate_builtin(times, 3).
determinate_builtin(divide, 4).
determinate_builtin(writef, 1).
determinate_builtin(writef, 2).
determinate_builtin(fwritef, 2).
determinate_builtin(fwritef, 3).
determinate_builtin(read_string, 2).
determinate_builtin(read_line, 1).
determinate_builtin(dif, 2).
determinate_builtin(unify_with_occurs_check, 2).
determinate_builtin(flush_output, 0).
determinate_builtin(flush_output, 1).
determinate_builtin(set_input, 1).
determinate_builtin(set_output, 1).
determinate_builtin(set_error_output, 1).
determinate_builtin(at_end_of_stream, 0).
determinate_builtin(at_end_of_stream, 1).
determinate_builtin(atom_chars, 2).
determinate_builtin(get_char, 1).
determinate_builtin(get_char, 2).
determinate_builtin(peek_char, 1).
determinate_builtin(peek_char, 2).
determinate_builtin(put_char, 1).
determinate_builtin(put_char, 2).
determinate_builtin(atom_codes, 2).
determinate_builtin(number_codes, 2).

determinate_builtin(NAME, ARITY) :-
	recorded(determinate, NAME/ARITY).
