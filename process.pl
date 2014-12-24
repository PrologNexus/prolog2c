%%% process toplevel forms


compile_file(FILE) :-
	initial_state(STATE),
	see(FILE), !,
	process_input([], [], _, STATE).
compile_file(FILE) :-
	error(['compilation of ', FILE, ' failed.']).

compile_file_finished(STATE) :-
	process_boilerplate_code(STATE).
compile_file_finished(STATE) :-
	process_initialization_goals(STATE).
compile_file_finished(STATE) :-
	seen,
	recorded(output_file, OUTFILE),
	(recorded(show_intermediate_code, yes) -> show_intermediate_code
	; assemble_file(OUTFILE, STATE)
	).


%% process next input, collecting a "block" of clauses of a given name/arity

% no more input, read next
process_input([], BLOCK, NA, STATE) :-
	read1(EXPR),
	process_input([EXPR], BLOCK, NA, STATE).

% end of file reached, compile block, if not empty
process_input([end_of_file], BLOCK, NA, STATE) :-
	open_file_stack(INFILE, STATE2, STATE), % note order
	seen,
	see(INFILE),
	process_input([], BLOCK, NA, STATE2).
process_input([end_of_file], [], _, STATE) :-
	compile_file_finished(STATE).
process_input([end_of_file], BLOCK, NA, STATE1) :-
	compile_block(NA, BLOCK, STATE1, STATE2),
	compile_file_finished(STATE2).

% detect DCG rules and expand
process_input([(HEAD --> BODY)|MORE], BLOCK, NA, STATE) :-
        dcg_rule((HEAD --> BODY), EXPANSION),
	%writeq(EXPANSION), nl,
	process_input([EXPANSION|MORE], BLOCK, NA, STATE).

% matches N/A? add and continue
process_input([(HEAD :- BODY)|MORE], BLOCK, NAME/ARITY, STATE) :-
	functor(HEAD, NAME, ARITY),
	!,			   
	process_input(MORE, [(HEAD :- BODY)|BLOCK], NAME/ARITY, STATE).

% otherwise compile current block and start new block
process_input([(HEAD :- BODY)|MORE], BLOCK, NA, STATE) :-
	functor(HEAD, NAME, ARITY),
	compile_block(NA, BLOCK, STATE, STATE2),
	!,
	process_input(MORE, [(HEAD :- BODY)], NAME/ARITY, STATE2).

% detect declarations
process_input([(:- DECL)|MORE], [], _, STATE) :-
	!, 
	process_directive(DECL, STATE, STATE2),
	process_input(MORE, [], _, STATE2).
process_input([(:- DECL)|MORE], BLOCK, NA, S) :-
        compile_block(NA, BLOCK, S, S1),
	!,
	process_directive(DECL, S1, S2),
	process_input(MORE, [], _, S2).

% the same for facts
process_input([HEAD|MORE], BLOCK, NAME/ARITY, STATE) :-
	functor(HEAD, NAME, ARITY),
	!,
	process_input(MORE, [HEAD|BLOCK], NAME/ARITY, STATE).

process_input([HEAD|MORE], BLOCK, NA, STATE) :-
	functor(HEAD, NAME, 0),
	!,
	compile_block(NA, BLOCK, STATE, STATE2),
	process_input(MORE, [HEAD], NAME/0, STATE2).

% otherwise invalid - if there is an active block, compile it first
process_input(INPUT, [C|CR], NA, STATE) :-
	compile_block(NA, [C|CR], STATE, STATE2),
	!,
	process_input(INPUT, [], _, STATE2).
process_input([EXPR|_], _, _, _) :-
	error(['invalid clause: ', EXPR]).


%% process a directive (declaration)

process_directive((DECL1, DECL2), S1, S2) :-
	process_directive(DECL1, S1, S),
	process_directive(DECL2, S, S2).

process_directive(initialization(GOAL), STATE, STATE) :-
	(recorded(initialization_goal, OLD, REF), erase(REF) ->
	 recorda(initialization_goal, (OLD, GOAL))
	; recorda(initialization_goal, GOAL)).

process_directive(pre_initialization(GOAL), STATE, STATE) :-
	(recorded(pre_initialization_goal, OLD, REF), erase(REF) ->
	 recorda(pre_initialization_goal, (OLD, GOAL))
	; recorda(pre_initialization_goal, GOAL)).

process_directive(include(FNAME), STATE1, STATE2) :-
	seeing(CURRENT), 
	open_file_stack(CURRENT, STATE1, STATE2),
	locate_file(FNAME, REALNAME),
	display('% including '), display(REALNAME), nl,
	see(REALNAME).

process_directive(global_variable(NAME), S, S) :-
	mangle_name(NAME, MNAME),
	recordz(global_variables, MNAME).

process_directive(op(P, A, N), S, S) :- op(P, A, N).

process_directive(DECL, STATE, STATE) :-
	error(['unrecognized directive: ', DECL]).


%% compile a block of clauses

compile_block(NA, BLOCK, STATE1, STATE2) :-
	reverse(BLOCK, RBLOCK),
	compile_clauses(NA, RBLOCK, STATE1, STATE2).


%% list generated intermediate code

show_intermediate_code :-
	retract(code(OP)),
	writeq(OP), put(46), nl,
	fail.
show_intermediate_code.


%% add clauses for boilerplate code and (pre-)initialization goals

process_boilerplate_code(STATE) :-
	findall(B, (recorded(boilerplate, B, REF), erase(REF)), BOILERPLATE),
	BOILERPLATE \= [],
	process_input(BOILERPLATE, [], _, STATE).

process_initialization_goals(STATE) :-
	\+recorded(initialization_done, _),
	(recorded(initialization_goal, GOAL); GOAL = main),
	(recorded(pre_initialization_goal, IGOAL); IGOAL = true),
	recorda(initialization_done, true),
	default_setting(entry_point, EP),
	process_input([(EP :- IGOAL, GOAL)], [], _, STATE).
