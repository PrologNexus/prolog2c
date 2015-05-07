%%%% generate cross-referencing information


emit_xref_information :-
	dump_directives,
	dump_defined_predicates,
	dump_unknown_predicates,
	dump_boilerplate_code.

dump_directives :-
	forall(recorded(directive, DECL),
	       (writeq(directive(DECL)), put_char('.'), nl) ).

dump_defined_predicates :-
	forall(recorded(defined, N/A),
	       (writeq(defined(N, A)), put_char('.'), nl) ).

dump_unknown_predicates :-
	forall(recorded(unresolved, N/A),
	       (writeq(unknown(N, A)), put_char('.'), nl) ).

dump_boilerplate_code :-
	forall(recorded(boilerplate, B),
	       (writeq(boilerplate(B)), put_char('.'), nl) ).


%% register defined or unresolved predicates

register_unresolved_call(NA) :-
	(recorded(defined, NA)
	; recorded(unresolved, NA)
	; recordz(unresolved, NA)
	).

register_defined_predicate(NA) :-
	(recorded(defined, NA)
	-> N/A = NA, error(['Non-contiguous predicate definition: ', N, '/', A])
	; recordz(defined, NA)
	),
	recorded(unresolved, NA, REF), erase(REF).
register_defined_predicate(_).


%% operations on the call-tree

%% FROM, TO: NAME/ARITY
register_call(FROM, FROM).
register_call(FROM, TO) :-
	recordz(calls, calls(FROM, TO)).

predicate_callers(NA, CALLERS) :-
	setof(CALLER, recorded(calls, calls(CALLER, NA)), LST),
	!,
	CALLERS = LST.
predicate_callers(_, []).
