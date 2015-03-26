%%%% Prolog-preprocessor (provides "main" entry-point)
%
% - does not process user-defined meta-predicate arguments.


:- ensure_loaded(library(dcg)).
:- discontiguous term_expansion/2, goal_expansion/2.


%% so at least one clause is defined
term_expansion(_, _) :- fail.

goal_expansion(_, _) :- fail.


expand_term(X, Y) :-
	term_expansion(X, Y1),
	!,
	( X == Y1
	-> Y = X
	; ( is_list(Y1)
	  -> expand_term_list(Y1, Y)
	  ; expand_term(Y1, Y)
	  )
	).
expand_term(X, X).

expand_term_list([], []).
expand_term_list([X|R1], [Y|R2]) :-
	expand_term(X, Y),
	expand_term_list(R1, R2).


expand_goal(X, Y) :-
	goal_expansion(X, Y1),
	!,
	(X == Y1 -> Y = X; expand_goal(Y1, Y)).
expand_goal(X, Y) :-
	functor(X, NAME, ARITY),
	expand_meta_goal(NAME/ARITY, X, Y).
expand_goal(X, X).

expand_meta_goal(catch/3, catch(X, B, Y), catch(X1, B, Y1)) :-
	expand_goal(X, X1),
	expand_goal(Y, Y1).
expand_meta_goal(N/3, X, Y) :-
	memberchk(N, [findall, setof, bagof]),
	!,
	X =.. [_, A, G, C],
	expand_goal(G, XG),
	Y =.. [N, A, XG, C].
expand_meta_goal(N/2, X, Y) :-
	memberchk(N, [delay, freeze]),
	!,
	X =.. [_, V, G],
	expand_goal(G, XG),
	Y =.. [N, V, XG].
expand_meta_goal(NA, X, Y) :-
	memberchk(NA, [','/2, ';'/2, '->'/2, forall/2, once/1, '/'('\\+', 1)]),
	!,
	X =.. [N|ARGS],
	expand_goal_list(ARGS, XARGS),
	Y =.. [N|XARGS].

expand_goal_list([], []).
expand_goal_list([X|R], [Y|R2]) :-
	expand_goal(X, Y),
	expand_goal_list(R, R2).


%% entry-point - reads current_input and writes expansions to current_output

main :-
	read(TERM),
	TERM \== end_of_file,
	process_term(TERM),
	!, main.
main.

process_term(TERM) :-
	expand_term(TERM, XTERM),
	( is_list(XTERM)
	-> forall(member(XT, XTERM), process_expanded_term(XT))
	; process_expanded_term(XTERM)
	).

process_expanded_term((HEAD --> BODY)) :-
        dcg_rule((HEAD --> BODY), X1),
	process_expanded_term(X1).
process_expanded_term((X :- Y)) :-
        expand_goal(Y, Y1),
	output_expanded_term((X :- Y1)).
process_expanded_term(TERM) :-
        output_expanded_term(TERM).

output_expanded_term(TERM) :-
	writeq(TERM), display('.\n\n').
