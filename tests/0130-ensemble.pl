%%% simple test for ensemble, using producer/consumer processes


:- ensure_loaded(library(boss)).


main :-
	spawn("tmp/producer", [], _),
	spawn("tmp/consumer", [], _),
	process_requests.
