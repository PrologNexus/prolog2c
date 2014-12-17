%%% compile.pl


%% compile a set of clauses

compile_clauses(NAME/ARITY, CLAUSES, S1, S2) :-
	emit(enter(NAME, ARITY)),
	compile_clause_list(CLAUSES, NAME/ARITY, 1, S1, S2).

compile_clause_list([CLAUSE], NAME/ARITY, I, S1, S2) :-
	clause_label(NAME, ARITY, I, L),
	emit(label(L)),
	(I > 1 -> emit(redo); true),
	compile_clause(CLAUSE, NAME/ARITY, I, last, S1, S2).	
compile_clause_list([CLAUSE|MORE], NAME/ARITY, I, S1, S2) :-
	clause_label(NAME, ARITY, I, L),
	emit(label(L)),
	compile_clause(CLAUSE, NAME/ARITY, I, notlast, S1, S),
	I2 is I + 1,
	compile_clause_list(MORE, NAME/ARITY, I2, S, S2).

% rule
compile_clause((HEAD :- BODY), NAME/ARITY, I, LAST, S1, S2) :-
        show_compiled_clause(HEAD :- BODY),
	!,			% avoid match of next clause
	I2 is I + 1,
	clause_label(NAME, ARITY, I2, L),
	compile_choice_point(LAST, L),
	gather_variables([HEAD, BODY], VARS),
	length(VARS, N),
	(N > 0 -> emit(environment(N)); true),
	compile_head(HEAD, BOUND, S1, S),
	compile_body(BODY, nondet, LAST, BOUND, S, S2),
	emit(exit).
% fact
compile_clause(HEAD, NA, I, M, S1, S2) :-
	compile_clause((HEAD :- true), NA, I, M, S1, S2).

show_compiled_clause(CLAUSE) :-
	recorded(show_compiled_clauses, yes),
	write('% '), writeq(CLAUSE), nl.
show_compiled_clause(_).


%% utilities

% perform CP-handling for a particular clause-position (no CP needed in last clause)
compile_choice_point(notlast, L) :- emit(add_choice_point(L)).
compile_choice_point(last, _) :- emit(pop_arguments).

% generate clause label from name/arity + index
clause_label(N, A, I, L) :-
	mangle_name(N, MN),
	atomic_list_concat([MN, '$', A, '_', I], L).


%% compile head-unification

compile_head(HEAD, BOUND, S1, S2) :-
	HEAD =.. [_|ARGS],
	compile_unification(ARGS, 0, [], BOUND, S1, S2).

% compile unification of argument
compile_unification([], _, BOUND, BOUND, S, S).
compile_unification([ARG|MORE], INDEX, BOUND1, BOUND2, S1, S2) :-
	compile_unification1(ARG, INDEX, BOUND1, BOUND, S1, S),
	INDEX2 is INDEX + 1,
	compile_unification(MORE, INDEX2, BOUND, BOUND2, S, S2).

% distinguish cases: bound/unbound variable or term (either constant or containing variable)
compile_unification1('_var_'(N), INDEX, BOUND, BOUND, S1, S2) :-
	member(N, BOUND),	% already bound?
	gensym('T', T1, S1, S),
	gensym('T', T2, S, S2),
	emit(local(N, T1), argument(INDEX, T2), unify(T1, T2)).
compile_unification1('_var_'(N), INDEX, BOUND, [N|BOUND], S1, S2) :-
	gensym('T', T1, S1, S2),
	emit(argument(INDEX, T1), assign(N, T1)).
compile_unification1(TERM, INDEX, BOUND1, BOUND2, S1, S2) :-
	gensym('T', T1, S1, S3),
	gensym('T', T2, S3, S4),
	compile_term_for_unification(TERM, T1, BOUND1, BOUND2, S4, S2),
	emit(argument(INDEX, T2), unify(T1, T2)).

% compile term, for unification, or for calls
compile_term_for_unification('_var_'(N), DEST, BOUND, BOUND, S, S) :-
	member(N, BOUND),	% already bound?
	emit(local(N, DEST)).
compile_term_for_unification('_var_'(N), DEST, BOUND, [N|BOUND], S, S) :-
	emit(make_variable(DEST), assign(N, DEST)).
compile_term_for_unification(X, DEST, BOUND, BOUND, S1, S2) :-
	literal_term(X), 	% literal term not containing variables?
	register_literal(X, N, S1, S2),
	emit(literal(N, DEST)).
compile_term_for_unification([X|Y], DEST, BOUND1, BOUND2, S1, S2) :-
	compile_term_arguments([X, Y], [], [CAR, CDR], BOUND1, BOUND2, S1, S2),
	emit(make_pair(CAR, CDR, DEST)).
compile_term_for_unification(X, DEST, BOUND1, BOUND2, S1, S2) :-
	X =.. LIST,
	compile_term_arguments(LIST, [], DLIST, BOUND1, BOUND2, S1, S2),
	emit(make_term(DLIST, DEST)).

% compile list of arguments, putting elements on stack
compile_term_arguments([], DL, RDL, B, B, S, S) :-
	reverse(DL, RDL).
compile_term_arguments([X|MORE], DL1, DL2, B1, B2, S1, S2) :-
	gensym('T', T, S1, S3),
	compile_term_for_unification(X, T, B1, B3, S3, S4),
	compile_term_arguments(MORE, [T|DL1], DL2, B3, B2, S4, S2).
	
			 
%% compile body

compile_body(BODY, DET, LAST, BOUND, S1, S2) :-
	compile_body_expression(BODY, tail, LAST, DET, _, BOUND, _, S1, S2).

% conjunction
compile_body_expression((X, Y), TAIL, LAST, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression(X, nontail, LAST, D1, D, B1, B, S1, S),
	compile_body_expression(Y, TAIL, LAST, D, D2, B, B2, S, S2).

% if-then-else
compile_body_expression((X -> Y; Z), TAIL, LAST, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(add_choice_point(L1), push_choice_points),
	compile_body_expression(X, nontail, LAST, D1, D3, B1, B3, S4, S5),
	emit(pop_choice_points),
	compile_body_expression(Y, TAIL, LAST, D3, D4, B3, B4, S5, S6),
	emit(jump(L2), label(L1)),
	compile_body_expression(Z, TAIL, LAST, D3, D5, B4, B2, S6, S2),
	emit(label(L2)),
	both_determinate(D4, D5, D2).

% disjunction
compile_body_expression((X; Y), TAIL, LAST, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(add_choice_point(L1)),
	compile_body_expression(X, nontail, LAST, D1, D3, B1, B, S4, S5),
	emit(jump(L2), label(L1)),
	compile_body_expression(Y, TAIL, LAST, D1, D4, B, B2, S5, S2),
	emit(label(L2)),
	both_determinate(D3, D4, D2).

% cut
compile_body_expression(!, _, last, _, det, B, B, S, S) :-
	emit(remove_choice_points).
compile_body_expression(!, _, notlast, _, det, B, B, S, S) :-
	emit(remove_choice_points, pop_arguments).

% true
compile_body_expression(true, _, _, D, D, B, B, S, S).

% fail
compile_body_expression(fail, _, _, D, D, B, B, S, S) :-
	emit(fail).

% repeat
compile_body_expression(repeat, _, _, det, det, B, B, S, S).
compile_body_expression(repeat, _, _, D, D, B, B, S1, S2) :-
	gen_label(L, S1, S2),
	emit(label(L), add_choice_point(L)).
	
% not
compile_body_expression(\+X, _, LAST, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	emit(push_choice_points, add_choice_point(L1)),
	compile_body_expression(X, nontail, LAST, D1, D2, B1, B2, S3, S2),
	emit(pop_choice_points, fail, label(L1)).

% if-then
compile_body_expression(X -> Y, TAIL, LAST, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression((X -> Y; fail), TAIL, LAST, D1, D2, B1, B2, S1, S2).

% inline-unification
compile_body_expression('_var_'(N) = '_var_'(N), _, _, D, D, B, B, S, S).
compile_body_expression('_var_'(N) = Y, _, _, D, D, B1, B2, S1, S2) :-
	\+member(N, B1), Y \= '_var_'(_),
	gensym('T', T, S1, S),
	compile_term_for_unification(Y, T, [N|B1], B2, S, S2),
	emit(assign(N, T)).
compile_body_expression(X = '_var_'(N), _, _, D, D, B1, B2, S1, S2) :-
	\+member(N, B1), X \= '_var_'(_),
	gensym('T', T, S1, S),
	compile_term_for_unification(X, T, [N|B1], B2, S, S2),
	emit(assign(N, T)).
compile_body_expression(X = Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(unify(T1, T2)).
compile_body_expression(X \= Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(not_unify(T1, T2)).

% identity comparison
compile_body_expression(X == Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(identical(T1, T2)).
compile_body_expression(X \== Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(not_identical(T1, T2)).

% arithmetic
compile_body_expression('_var_'(N) is EXP, _, _, D, D, B1, [N|B1], S1, S2) :-
	\+member(N, B1),
	gensym('T', T1, S1, S),
	compile_arithmetic_expression(EXP, T1, B1, S, S2),
	emit(assign(N, T1)).
compile_body_expression(X is EXP, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_arithmetic_expression(EXP, T1, B1, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(X, T2, B1, B2, S5, S2),
	emit(unify(T1, T2)).

compile_body_expression(X =:= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_equal, S1, S2).
compile_body_expression(X =\= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_not_equal, S1, S2).
compile_body_expression(X > Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_greater, S1, S2).
compile_body_expression(X < Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_less, S1, S2).
compile_body_expression(X >= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_greater_or_equal, S1, S2).
compile_body_expression(X =< Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_less_or_equal, S1, S2).
			

% foreign call
compile_body_expression(foreign_call(CALL), _, _, D, D, B1, B2, S1, S2) :-
	CALL =.. [NAME|ARGS],
	compile_term_arguments(ARGS, [], DLIST, B1, B2, S1, S2),
	emit(foreign_call(NAME, DLIST)).

% type-predicates

% ordinary predicate call
compile_body_expression('_var_'(N), TAIL, LAST, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression(call('_var_'(N)), TAIL, LAST, D1, D2, B1, B2, S1, S2).
compile_body_expression(TERM, TAIL, _, D, D, B1, B2, S1, S2) :-
	TERM =.. [NAME|ARGS],
	compile_term_arguments(ARGS, [], DLIST, B1, B2, S1, S),
	(compile_type_predicate(NAME, DLIST), S2 = S
	; DLIST = [X, Y], compile_order_predicate(NAME, X, Y), S2 = S
	; compile_ordinary_call(NAME, TAIL, DLIST, D, S, S2)
	).

compile_body_expression(TERM, _, _, _, _, _, _, _, _) :-
	error(['can not compile: ', TERM]).


%% compile calls (ordinary or type-/order-predicate)

compile_ordinary_call(NAME, TAIL, DLIST, D, S1, S2) :-
	 gen_label(L, S1, S2),
	 (TAIL/D = tail/det -> emit(tailcall(NAME, DLIST))
	 ; emit(call(NAME, DLIST, L))
	 ).

compile_type_predicate(NAME, [VAL]) :-
	type_predicate(NAME),
	CALL =.. [NAME, VAL],
	emit(CALL).

type_predicate(number).
type_predicate(atomic).
type_predicate(atom).
type_predicate(number).
type_predicate(integer).
type_predicate(compound).
type_predicate(float).
type_predicate(var).
type_predicate(nonvar).

compile_order_predicate('@<', X, Y) :- emit(term_less(X, Y)).
compile_order_predicate('@>', X, Y) :- emit(term_less(Y, X)).
compile_order_predicate('@>=', X, Y) :- emit(term_not_less(X, Y)).
compile_order_predicate('@=<', X, Y) :- emit(term_not_less(Y, X)).


%% helper predicates for arithmetic expressions

compile_arithmetic_test(X, Y, B, OP, S1, S2) :-
	compile_arithmetic_operation_arguments([X, Y], [T1, T2], B, S1, S2),
	TERM =.. [OP, T1, T2],
	emit(TERM).

compile_arithmetic_expression('_var_'(N), DEST, B, S, S) :-
	member(N, B),
	emit(local(N, DEST)).
compile_arithmetic_expression('_var_'(N), _, _, _, _) :-
	error(['unbound variable in arithmetic expression: ', N]). %XXX
compile_arithmetic_expression(X, DEST, _, S1, S2) :-
	number(X),
	register_literal(X, N, S1, S2),
	emit(literal(N, DEST)).

compile_arithmetic_expression(EXP, DEST, B, S1, S2) :-
	functor(EXP, NAME, ARITY),
	EXP =.. [_|ARGS],
	arithmetic_operation(NAME, ARITY, OP),
	compile_arithmetic_operation_arguments(ARGS, DLIST, B, S1, S2),
	append(DLIST, [DEST], DLIST2),
	FN =.. [OP|DLIST2],
	emit(FN).

compile_arithmetic_expression(X, _, _, _, _) :-
	error(['invalid arithmetic expression: ', X]).

arithmetic_operation(abs, 1).
arithmetic_operation(atan, 1).
arithmetic_operation(ceiling, 1).
arithmetic_operation(cos, 1).
arithmetic_operation(exp, 1).
arithmetic_operation(float, 1).
arithmetic_operation(float_fractional_part, 1).
arithmetic_operation(float_integer_part, 1).
arithmetic_operation(floor, 1).
arithmetic_operation(log, 1).
arithmetic_operation(mod, 2).
arithmetic_operation(rem, 2).
arithmetic_operation(round, 1).
arithmetic_operation(sign, 1).
arithmetic_operation(sin, 1).
arithmetic_operation(sqrt, 1).
arithmetic_operation(truncate, 1).

arithmetic_operation('+', 2, add).
arithmetic_operation('/\\', 2, bitwise_and).
arithmetic_operation('\\', 1, bitwise_not).
arithmetic_operation('<<', 2, shift_left).
arithmetic_operation('>>', 2, shift_right).
arithmetic_operation('\\/', 1, bitwise_or).
arithmetic_operation('**', 1, exponent).
arithmetic_operation('/', 2, divide).
arithmetic_operation('-', 1, negate).
arithmetic_operation('-', 2, subtract).
arithmetic_operation('//', 2, quotient).
arithmetic_operation('*', 2, multiply).

arithmetic_operation(NAME, ARITY, NAME) :- arithmetic_operation(NAME, ARITY).

compile_arithmetic_operation_arguments([ARG], [T], B, S1, S2) :-
	gensym('T', T, S1, S),
	compile_arithmetic_expression(ARG, T, B, S, S2).

compile_arithmetic_operation_arguments([ARG1, ARG2], [T1, T2], B, S1, S2) :-
	compile_arithmetic_operation_arguments([ARG1], [T1], B, S1, S),
	compile_arithmetic_operation_arguments([ARG2], [T2], B, S, S2).

	
%% adding code to compiled-code-database

emit(T) :- assertz(code(T)).
emit(T1, T2) :- emit(T1), emit(T2).
emit(T1, T2, T3) :- emit(T1, T2), emit(T3).
emit(T1, T2, T3, T4) :- emit(T1, T2, T3), emit(T4).
emit(T1, T2, T3, T4, T5) :- emit(T1, T2, T3, T4), emit(T5).


%% check if both determinate-flags are set

both_determinate(det, det, det).
both_determinate(_, _, nondet).


%% register literal data

register_literal(TERM, N, S, S) :- clause(literal(N, TERM), _), !.
register_literal(TERM, N, S1, S2) :-
	gen_literal_index(N, S1, S2),
	assertz(literal(N, TERM)).
