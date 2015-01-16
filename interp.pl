%%%% interpeter core

%% based on:

%   File   : INTERP
%   Author : R.A.O'Keefe
%   Updated: 2 March 84
%   Purpose: Meta-circular interpreter for Prolog

%% ... but heavily modified.

/*  This is a genuinely meta-circular interpreter for a subset of Prolog
    containing cuts.  It relies on the fact that disjunction is transparent
    to cut just like conjunction.  If it doesn't work in your Prolog, and
    if you paid more than $100 for it, take your Prolog back to the shop
    and insist that they fix it, there are at least four different ways of
    implementing disjunction so that it works.
*/

:- global_variable(trace_depth).


do_goal(Goal) :-
	system(Goal),		% <--- check for a built in predicate
	!,
	call(Goal).
do_goal(Goal) :-
	clause(Goal, Body),	% <--- assume anything else is interpreted
	do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).

do_body(Body) :-
	do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).

do_body((Conj1,Conj2), AfterCut, HadCut) :- !,
	do_body(Conj1, Conj2, AfterCut, HadCut).
do_body(!, true, yes) :- !.
do_body((Disj1;_), AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_body((_;Disj2), AfterCut, HadCut) :- !,
	do_body(Disj2, AfterCut, HadCut).
do_body(true, true, no) :- !.
do_body(Goal, true, no) :-
	do_goal(Goal).


do_body(!, AfterCut, AfterCut, yes) :- !.
do_body((A,B), Conj, AfterCut, HadCut) :- !,
	do_body(A, (B,Conj), AfterCut, HadCut).
do_body((Disj1;_), Conj, AfterCut, HadCut) :-
	do_body(Disj1, Conj, AfterCut, HadCut).
do_body((_;Disj2), Conj, AfterCut, HadCut) :- !,
	do_body(Disj2, Conj, AfterCut, HadCut).
do_body(doue, Body, AfterCut, HadCut) :- !,
	do_body(Body, AfterCut, HadCut).
do_body(Goal, Body, AfterCut, HadCut) :-
	do_goal(Goal),
	do_body(Body, AfterCut, HadCut).


trace(Goal) :-
	tr_goal(Goal, 0).

tr_goal(call(Goal), Depth) :- !,
	nonvar(Goal),
	tr_body(Goal, Depth).
tr_goal(\+(Goal), Depth) :-
	tr_body(Goal, Depth),
	!, fail.
tr_goal(\+(Goal), Depth) :- !.
tr_goal(Goal, Depth) :-
	(   trace_out(Depth, 'Call: ', Goal)
	;   Depth1 is 1+Depth,
	    tr_call(Goal, Depth1),
	    (   trace_out(Depth, 'Exit: ', Goal)
	    ;	true
	    ;   trace_out(Depth, 'Redo: ', Goal)
	    )
	;   trace_out(Depth, 'Fail: ', Goal)
	).


tr_call(Goal, Depth) :-
	system(Goal),
	!,
	global_set(trace_depth, Depth),
	call(Goal).
tr_call(Goal, Depth) :-
	clause(Goal, Body),
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		trace_out(Depth, 'CUT'),
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).


tr_body(Body, Depth) :-
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		trace_out(Depth, 'CUT'),
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).


tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
tr_body(!, _, true, yes) :- !.
tr_body((Disj1;_), Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Depth, AfterCut, HadCut).
tr_body(true, _, true, no) :- !.
tr_body(Goal, Depth, true, no) :-
	tr_goal(Goal, Depth).

tr_body(!, AfterCut, _, AfterCut, yes) :- !.
tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
tr_body((Disj1;_), Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Conj, Depth, AfterCut, HadCut).
tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	tr_body(Body, Depth, AfterCut, HadCut).
tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	tr_goal(Goal, Depth),
	tr_body(Body, Depth, AfterCut, HadCut).


%%

trace_out(T, M) :-
	telling(OLD),
	current_error_output(ERR),
	tell(ERR),
	tab(T), display(M), nl,
	tell(OLD), !.

trace_out(T, M, G) :-
	telling(OLD),
	current_error_output(ERR),
	tell(ERR),
	tab(T), display(M), writeq(G), nl,
	tell(OLD),
	!, fail.


%%

execute(GOAL) :-
	global_ref(trace_depth, N),
	integer(N),
	!,
	tr_body(GOAL, N).
execute(GOAL) :- do_body(GOAL).

system(TERM) :-
	functor(TERM, NAME, ARITY),
	system_predicate(NAME, ARITY).

:- include(system_predicate).

system_predicate(trace, 0).
system_predicate(call, 1).
system_predicate(consult, 1).
system_predicate(forall, 2).
system_predicate(findall, 3).
system_predicate(catch, 3).
system_predicate(repeat, 0).
system_predicate('->', 2).
system_predicate('\\+', 1).

call(TERM) :-
	!,
	functor(TERM, NAME, ARITY),
	call_primitive(NAME, ARITY, TERM).

:- include(call_primitive).

call_primitive(call, 1, TERM) :- !, arg(1, TERM, X), execute(X).
call_primitive(trace, 0, TERM) :-
	!,
	global_ref(trace_depth, D),
	(integer(D) -> global_set(trace_depth, none)
	; global_set(trace_depth, 0)).
call_primitive(consult, 1, TERM) :-
	!,
	arg(1, TERM, X), consult(X).
call_primitive(forall, 2, TERM) :-
	!,
	arg(1, TERM, G), arg(2, TERM, A),
	forall(execute(G), execute(A)).
call_primitive(findall, 3, TERM) :-
	!,
	arg(1, TERM, T), arg(2, TERM, G), arg(3, TERM, R),
	findall(T, execute(G), R).
call_primitive(catch, 3, TERM) :-
	!,
	arg(1, TERM, G), arg(2, TERM, B), arg(3, TERM, R),
	catch(execute(G), B, execute(R)).
call_primitive('->', 2, TERM) :-
	!,
	arg(1, TERM, X), arg(2, TERM, Y),
	execute(X) -> execute(Y).
call_primitive('\\+', 1, TERM) :-
	!,
	arg(1, TERM, X),
	\+execute(X).
call_primitive(repeat, 0, _) :- do_repeat.

do_repeat.
do_repeat :- do_repeat.


%%

evaluate(X, Y) :-
	(atom(X); compound(X)),
	!,
	functor(X, NAME, ARITY),
	evaluate_op(NAME, ARITY, X, Y).
evaluate(X, X) :- number(X), !.
evaluate(X, _) :- throw(type_error(number, X)).
evaluate(X, _) :- throw(instantiation_error).

:- include('evaluate_op.pl').

evaluate_op(_, _, TERM, _) :-
	throw(error('invalid arithmetic expression', TERM)).


%%

consult(FILE) :-
	find_file(FILE, FILE2),
	seeing(OLD),
	see(FILE2),
	consult_terms(0/0),
	seen,
	see(OLD).

consult_terms(PNA) :-
	read(TERM),
	TERM \== end_of_file,
	insert_term(PNA, TERM, CNA),
	!,
	consult_terms(CNA).
consult_terms(_).

find_file(FILE, FILE) :-
	exists_file(FILE), !.
find_file(FILE, FILE2) :-
	atom_codes(FILE, STR),
	append(STR, ".pl", STR2),
	atom_codes(FILE2, STR2),
	exists_file(FILE2), !.

insert_term(PNA, (:- BODY), PNA) :-
        !,
	process_directive(BODY).
insert_term(PNA, (HEAD :- BODY), N/A) :-
        functor(HEAD, N, A),
	!,
	add_clause(PNA, N, A, HEAD, BODY).
insert_term(PNA, FACT, N/A) :-
        functor(FACT, N, A),
	!,
	add_clause(PNA, N, A, FACT, true).

process_directive((X, Y)) :-
	process_directive(X),
	process_directive(Y).
process_directive(X) :-
	(execute(X); seen, throw(error('latent goal failed', X))).

add_clause(N/A, N, A, HEAD, BODY) :-
	(atom(HEAD); compound(HEAD))
	-> assertz((HEAD :- BODY))
	; throw(error('invalid clause head', HEAD)). 
add_clause(PN/PA, N, A, HEAD, BODY) :-
	(abolish(N/A); true),
	add_clause(PN/PA, PN, PA, HEAD, BODY).
