%%%% interpeter core

%% based on:

%   File   : INTERP
%   Author : R.A.O'Keefe
%   Updated: 2 March 84
%   Purpose: Meta-circular interpreter for Prolog

/*  This is a genuinely meta-circular interpreter for a subset of Prolog
    containing cuts.  It relies on the fact that disjunction is transparent
    to cut just like conjunction.  If it doesn't work in your Prolog, and
    if you paid more than $100 for it, take your Prolog back to the shop
    and insist that they fix it, there are at least four different ways of
    implementing disjunction so that it works.
*/

%% ... but heavily modified.

:- global_variable(pi_trace_depth).
:- pre_initialization(global_set(pi_trace_depth, none)).


pi_init :-
	assertz(term_expansion(X, X)),
	recordz(pi_silent, yes).

pi_do_goal(Goal) :-
	system(Goal),		% <--- check for a built in predicate
	!,
	call_system_predicate(Goal).
pi_do_goal(Goal) :-
	pi_clause(Goal, Body),	% <--- assume anything else is interpreted
	pi_do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		pi_do_body(AfterCut)
	;   HadCut = no
	).

pi_do_body(Body) :-
	pi_do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		pi_do_body(AfterCut)
	;   HadCut = no
	).

pi_do_body((Conj1,Conj2), AfterCut, HadCut) :- !,
	pi_do_body(Conj1, Conj2, AfterCut, HadCut).
pi_do_body(!, true, yes) :- !.
pi_do_body((Disj1;_), AfterCut, HadCut) :-
	pi_do_body(Disj1, AfterCut, HadCut).
pi_do_body((_;Disj2), AfterCut, HadCut) :- !,
	pi_do_body(Disj2, AfterCut, HadCut).
pi_do_body(true, true, no) :- !.
pi_do_body(Goal, true, no) :-
	pi_do_goal(Goal).


pi_do_body(!, AfterCut, AfterCut, yes) :- !.
pi_do_body((A,B), Conj, AfterCut, HadCut) :- !,
	pi_do_body(A, (B,Conj), AfterCut, HadCut).
pi_do_body((Disj1;_), Conj, AfterCut, HadCut) :-
	pi_do_body(Disj1, Conj, AfterCut, HadCut).
pi_do_body((_;Disj2), Conj, AfterCut, HadCut) :- !,
	pi_do_body(Disj2, Conj, AfterCut, HadCut).
pi_do_body(doue, Body, AfterCut, HadCut) :- !,
	pi_do_body(Body, AfterCut, HadCut).
pi_do_body(Goal, Body, AfterCut, HadCut) :-
	pi_do_goal(Goal),
	pi_do_body(Body, AfterCut, HadCut).


trace(Goal) :-
	pi_tr_goal(Goal, 0).

pi_tr_goal(call(Goal), Depth) :- !,
	nonvar(Goal),
	pi_tr_body(Goal, Depth).
pi_tr_goal(\+(Goal), Depth) :-
	pi_tr_body(Goal, Depth),
	!, fail.
pi_tr_goal(\+(Goal), Depth) :- !.
pi_tr_goal(Goal, Depth) :-
	(   pi_trace_out(Depth, 'Call: ', Goal)
	;   Depth1 is 1+Depth,
	    pi_tr_call(Goal, Depth1),
	    (   pi_trace_out(Depth, 'Exit: ', Goal)
	    ;	true
	    ;   pi_trace_out(Depth, 'Redo: ', Goal)
	    )
	;   pi_trace_out(Depth, 'Fail: ', Goal)
	).


pi_tr_call(Goal, Depth) :-
	system(Goal),
	!,
	global_set(pi_trace_depth, Depth),
	call_system_predicate(Goal).
pi_tr_call(Goal, Depth) :-
	pi_clause(Goal, Body),
	pi_tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		pi_trace_out(Depth, 'CUT'),
		pi_tr_body(AfterCut, Depth)
	;   HadCut = no
	).


pi_tr_body(Body, Depth) :-
	pi_tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		pi_trace_out(Depth, 'CUT'),
		pi_tr_body(AfterCut, Depth)
	;   HadCut = no
	).


pi_tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	pi_tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
pi_tr_body(!, _, true, yes) :- !.
pi_tr_body((Disj1;_), Depth, AfterCut, HadCut) :-
	pi_tr_body(Disj1, Depth, AfterCut, HadCut).
pi_tr_body((_;Disj2), Depth, AfterCut, HadCut) :- !,
	pi_tr_body(Disj2, Depth, AfterCut, HadCut).
pi_tr_body(true, _, true, no) :- !.
pi_tr_body(Goal, Depth, true, no) :-
	pi_tr_goal(Goal, Depth).

pi_tr_body(!, AfterCut, _, AfterCut, yes) :- !.
pi_tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	pi_tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
pi_tr_body((Disj1;_), Conj, Depth, AfterCut, HadCut) :-
	pi_tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
pi_tr_body((_;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
	pi_tr_body(Disj2, Conj, Depth, AfterCut, HadCut).
pi_tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	pi_tr_body(Body, Depth, AfterCut, HadCut).
pi_tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	pi_tr_goal(Goal, Depth),
	pi_tr_body(Body, Depth, AfterCut, HadCut).


%%

pi_trace_out(T, M) :-
	telling(OLD),
	current_error_output(ERR),
	tell(ERR),
	tab(T), display(M), nl,
	tell(OLD), !.

pi_trace_out(T, M, G) :-
	telling(OLD),
	current_error_output(ERR),
	tell(ERR),
	tab(T), display(M), writeq(G), nl,
	tell(OLD),
	!, fail.


%%

call(GOAL) :-
	global_ref(pi_trace_depth, N),
	integer(N),
	!,
	pi_tr_body(GOAL, N).
call(GOAL) :- pi_do_body(GOAL).

system(TERM) :-
	functor(TERM, NAME, ARITY),
	pi_system_predicate(NAME, ARITY).

:- include('pi_system_predicate.pl').

pi_system_predicate(trace, 0).
pi_system_predicate(call, 1).
pi_system_predicate(consult, 1).
pi_system_predicate(forall, 2).
pi_system_predicate(findall, 3).
pi_system_predicate(bagof, 3).
pi_system_predicate(setof, 3).
pi_system_predicate(catch, 3).
pi_system_predicate(repeat, 0).
pi_system_predicate('->', 2).
pi_system_predicate('\\+', 1).
pi_system_predicate('$call_predicate', 2).
pi_system_predicate(expand_term, 2).
pi_system_predicate(delay, 2).
pi_system_predicate(freeze, 2).
pi_system_predicate(dif, 2).

call_system_predicate(TERM) :-
	!,
	functor(TERM, NAME, ARITY),
	pi_call_primitive(NAME, ARITY, TERM).

:- include('pi_call_primitive.pl').

pi_call_primitive(call, 1, TERM) :- !, arg(1, TERM, X), call(X).
pi_call_primitive(trace, 0, TERM) :-
	!,
	global_ref(pi_trace_depth, D),
	(integer(D) -> global_set(pi_trace_depth, none)
	; global_set(pi_trace_depth, 0)).
pi_call_primitive(consult, 1, TERM) :-
	!,
	arg(1, TERM, X), consult(X).
pi_call_primitive(forall, 2, TERM) :-
	!,
	arg(1, TERM, G), arg(2, TERM, A),
	forall(call(G), call(A)).
pi_call_primitive(findall, 3, TERM) :-
	!,
	arg(1, TERM, T), arg(2, TERM, G), arg(3, TERM, R),
	findall(T, call(G), R).
pi_call_primitive(catch, 3, TERM) :-
	!,
	arg(1, TERM, G), arg(2, TERM, B), arg(3, TERM, R),
	catch(call(G), B, call(R)).
pi_call_primitive('->', 2, TERM) :-
	!,
	arg(1, TERM, X), arg(2, TERM, Y),
	(call(X) -> call(Y)).
pi_call_primitive('\\+', 1, TERM) :-
	!,
	arg(1, TERM, X),
	\+call(X).
pi_call_primitive(repeat, 0, _) :- pi_do_repeat.
pi_call_primitive(bagof, 3, TERM) :- pi_bagof_setof(bagof, TERM).
pi_call_primitive(setof, 3, TERM) :- pi_bagof_setof(setof, TERM).
pi_call_primitive(expand_term, 2, TERM) :-
	!, arg(1, TERM, X), arg(2, TERM, Y),
	expand_term(X, Y).

pi_call_primitive('$call_predicate', 2, TERM) :-
	!, arg(1, TERM, PTR), arg(2, TERM, ARGS),
	'$call_predicate'(PTR, ARGS).

pi_call_primitive(delay, 2, TERM) :-
	!, arg(1, TERM, VAR), arg(2, TERM, GOAL),
	delay(VAR, call(GOAL)).

pi_call_primitive(freeze, 2, TERM) :-
	!, arg(1, TERM, VAR), arg(2, TERM, GOAL),
	freeze(VAR, call(GOAL)).

pi_call_primitive(dif, 2, TERM) :-
	!, arg(1, TERM, X), arg(2, TERM, Y),
	dif(X, Y).

pi_bagof_setof(OP, TERM) :-
	!,
	arg(1, TERM, T), arg(2, TERM, G), arg(3, TERM, R),
	pi_free_variables(G, T, [], VARS),
	pi_drop_qualifiers(G, G2),
	pi_bagof(T, G2, VARS, R1),
	(OP == bagof -> R = R1; sort(R1, R)).


%% for repeat/0

pi_do_repeat.
pi_do_repeat :- pi_do_repeat.


%%

pi_evaluate(X, Y) :-
	(atom(X); compound(X)),
	!,
	functor(X, NAME, ARITY),
	pi_evaluate_op(NAME, ARITY, X, Y).
pi_evaluate(X, X) :- number(X), !.
pi_evaluate(X, _) :- throw(type_error(number, X)).
pi_evaluate(X, _) :- throw(instantiation_error).

:- include('pi_evaluate_op.pl').

pi_evaluate_op(_, _, TERM, _) :-
	throw(error('invalid arithmetic expression', TERM)).


%%

expand_term(TERM1, TERM2) :-
	call(term_expansion(TERM1, TERM2)), !.
expand_term(TERM, TERM).


%%

consult(FILE) :-
	pi_find_file(FILE, FILE2),
	seeing(OLD),
	see(FILE2),
	recordz(pi_loaded, FILE2),
	pi_msg('% consulting %q ...\n', FILE2),
	pi_consult_terms(0/0),
	seen,
	see(OLD).

pi_consult_terms(PNA) :-
	read(TERM),
	TERM \== end_of_file,
	expand_term(TERM, TERM2),
	pi_insert_tem(PNA, TERM2, CNA),
	!,
	pi_consult_terms(CNA).
pi_consult_terms(PNA) :-
	recorded(pi_include_file_stack, [NEXT|MORE], REF),
	erase(REF),
	recordz(pi_include_file_stack, MORE),
	seen,
	see(NEXT),
	!,
	pi_consult_terms(PNA).
pi_consult_terms(_).

pi_find_file(FILE, FILE2) :-
	\+atom(FILE), atom_codes(FILE, F2), pi_find_file(F2, FILE2).
pi_find_file(FILE, FILE) :-
	exists_file(FILE), !.
pi_find_file(FILE, FILE2) :-
	atom_codes(FILE, STR),
	append(STR, ".pl", STR2),
	atom_codes(FILE2, STR2),
	exists_file(FILE2), !.
pi_find_file(FILE, _) :-
	throw(existence_error(FILE)).

pi_insert_tem(PNA, (:- BODY), PNA) :-
        !,
	pi_process_directive(BODY).
pi_insert_tem(PNA, (HEAD :- BODY), N/A) :-
        functor(HEAD, N, A),
	!,
	pi_add_clause(PNA, N, A, HEAD, BODY).
pi_insert_tem(PNA, FACT, N/A) :-
        functor(FACT, N, A),
	!,
	pi_add_clause(PNA, N, A, FACT, true).

pi_process_directive((X, Y)) :-
	pi_process_directive(X),
	pi_process_directive(Y).
pi_process_directive(initialization(G)) :-
	(recorded(pi_initialization_goal, G1, REF)
	-> erase(REF), recordz(pi_initialization_goal, (G1, G))
	; recordz(pi_initialization_goal, G)
	).
pi_process_directive(ensure_loaded(FILE)) :-
	pi_find_file(FILE, FILE2),
	(recorded(pi_loaded, FILE2); pi_process_directive(include(FILE2))).
pi_process_directive(include(FILE)) :-
	pi_find_file(FILE, FILE2),
	seeing(CURRENT),
	(recorded(pi_include_file_stack, OLD, REF)	-> erase(REF); OLD = []),
	recordz(pi_include_file_stack, [CURRENT|OLD]),
	see(FILE2),
	recordz(pi_loaded, FILE2),
	pi_msg('% including %q ...\n', FILE2).
pi_process_directive(X) :-
	(call(X); seen, pi_close_all_files, throw(error('latent goal failed', X))).

pi_close_all_files :-
	recorded(pi_include_file_stack, STACK, REF),
	erase(REF),
	member(FILE, STACK), see(FILE), seen, fail.
pi_close_all_files.


%% clause lookup + assert

pi_clause(HEAD, BODY) :-
	'$fast_clause_lookup'(HEAD, REF1),
	!,
	'$clause_match'(REF1, (HEAD :- BODY), REF).
pi_clause(HEAD, _) :-
	functor(HEAD, N, A),
	throw(unknown(N/A)).

pi_add_clause(N/A, N, A, HEAD, BODY) :-
	((atom(HEAD); compound(HEAD))
	-> assertz((HEAD :- BODY))
	; throw(error('invalid clause head', HEAD))
	). 
pi_add_clause(PN/PA, N, A, HEAD, BODY) :-
	(abolish(N/A); true),
	pi_add_clause(PN/PA, PN, PA, HEAD, BODY).


%%% support code for bagof/setof

pi_bagof(T, G, [], R) :-
	!,
	findall(T, call(G), R),
	R \== [].
pi_bagof(T, G, VARS, _) :-
	'$bagof_start_unbound'(VARS, T, T2),
	call(G),
	'$findall_push'(T2),
	fail.
pi_bagof(_, _, _, R) :-
	'$bagof_finish'(R).


%% drop existential qualifiers from expression

pi_drop_qualifiers(_^X, Y) :- !, pi_drop_qualifiers(X, Y).
pi_drop_qualifiers(X, X).


%% from setof.pl in the DEC10 library:

pi_free_variables(Term, Bound, VarList, [Term|VarList]) :-
	var(Term),
	pi_term_is_free_of(Bound, Term),
	pi_list_is_free_of(VarList, Term),
	!.
pi_free_variables(Term, Bound, VarList, VarList) :-
	var(Term),
	!.
pi_free_variables(Term, Bound, OldList, NewList) :-
	pi_explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	pi_free_variables(NewTerm, NewBound, OldList, NewList).
pi_free_variables(Term, Bound, OldList, NewList) :-
	functor(Term, _, N),
	pi_free_variables(N, Term, Bound, OldList, NewList).

pi_free_variables(0, Term, Bound, VarList, VarList) :- !.
pi_free_variables(N, Term, Bound, OldList, NewList) :-
	arg(N, Term, Argument),
	pi_free_variables(Argument, Bound, OldList, MidList),
	M is N-1, !,
	pi_free_variables(M, Term, Bound, MidList, NewList).

%%   explicit_binding checks for goals known to existentially quantify
%%   one or more variables.  In particular \+ is quite common.

pi_explicit_binding(\+ Goal,	       Bound, fail,	Bound      ) :- !.
pi_explicit_binding(not(Goal),	       Bound, fail,	Bound	   ) :- !.
pi_explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var) :- !.
pi_explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
pi_explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.

pi_term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
pi_term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	pi_term_is_free_of(N, Term, Var).

pi_term_is_free_of(0, Term, Var) :- !.
pi_term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	pi_term_is_free_of(Argument, Var),
	M is N-1, !,
	pi_term_is_free_of(M, Term, Var).

pi_list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	!,
	pi_list_is_free_of(Tail, Var).
pi_list_is_free_of([], _).


%% messages

pi_msg(FMT) :- pi_msg(FMT, ARGS).
pi_msg(FMT, ARGS) :-
	(recorded(pi_silent, _); writef(FMT, ARGS)), !.
