%%% consumer example for ensemble -*- Prolog -*-


:- ensure_loaded(library(ensemble)).


main :-
	repeat,
	mread(counter(I)),
	fwritef(user_error, "%d\n", [I]),
	I == 10,
	display(user_error, 'finishing...'),
	flush_output(user_error),
	mwrite(consumed(done)),
	mterminate, % disconnect
	sleep(10). % let producer terminate, to avoid SIGCHILD interrupting poll
