%%%% support predicates for freeze/2


'$delay_goal'(VAR, PTR, ARGS) :-
	var(VAR) -> foreign_call(delay_goal(VAR, PTR, ARGS))
	; '$call_predicate'(PTR, ARGS).

'$defrost'(VAR, PTR, ARGS) :-
	writef('defrost: %t, %t, %t\n', [VAR, PTR, ARGS]),
	var(VAR) ->
	'$predicate_address'('$defrost'/3, PTR2),
	delay(VAR, '$call_predicate'(PTR2, [PTR, ARGS]))
	; '$call_predicate'(PTR, ARGS).
