%%%% support predicates for freeze/2


'$delay_goal'(VAR, PTR, ARGS) :-
	%display([delay, VAR, PTR, ARGS]), nl,
	(var(VAR) -> foreign_call(delay_goal(VAR, PTR, ARGS))
	; '$call_predicate'(PTR, ARGS)
	).

'$freeze_goal'(VAR, PTR, ARGS) :-
	%display([freeze, VAR, PTR, ARGS]), nl,
	(var(VAR) ->
	 '$predicate_address'('$freeze_goal'/3, PTR2),
	 delay(VAR, '$call_predicate'(PTR2, [VAR, PTR, ARGS]))
	; '$call_predicate'(PTR, ARGS)
	).

dif(X, Y) :-
	foreign_call(special_id(X, Y, V)),
	!,
	(var(V) -> delay(V, dif(X, Y))).
dif(_, _).
