%%% producer example for ensemble -*- Prolog -*-


:- ensure_loaded(library(ensemble)).


main :-
	between(1, 100, I),	% only 1-10 are consumed
	mwrite(counter(I)),
	mreadp(consumed(done)),
	display(user_error, 'done\n'),
	mterminate,
	sleep(10).		% avoid EINTR in poll (ugly)
