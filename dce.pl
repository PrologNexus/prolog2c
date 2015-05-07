%%% eliminate unused predicates


%% iterate over defined predicates and collect callers, dropping the
%% predicate if none exists.
mark_unused_predicates :-
	mark_unused_predicates(0).

mark_unused_predicates(COUNT) :-
	recorded(defined, NA, REF),
	predicate_callers(NA, []),
	erase(REF),
	eliminate_predicate(NA),
	C2 is COUNT + 1,
	!,
	mark_unused_predicates(C2).
mark_unused_predicates(COUNT) :-
	message(['% dropped ', COUNT, ' unused predicates']).

eliminate_predicate(NA) :-
	recorded(calls, calls(NA, _), REF),
	erase(REF),
	fail.
eliminate_predicate(_).
