%%%% support predicates for freeze/2


'$freeze_var'(VAR, PTR, ARGS) :-
	var(VAR) -> foreign_call(freeze_goal(VAR, PTR, ARGS))
	; '$call_predicate'(PTR, ARGS).
