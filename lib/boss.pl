%%%% master (server) part of "ensemble"


:- ensure_loaded(library(spawn)).
:- ensure_loaded(library(ensemble)).


process_requests :-
	log_event("start"),
	process_requests_loop.

process_requests_loop :-
	recorded(spawned_children, PS),
	PS \= [],
	findall(FDIN, member(process(_, FDIN, _), PS), WL),
	poll_fds(WL, RL),
	findall(P, (member(FD, RL), member(P, PS), P = process(_, FD, _)), READY),
	forall(member(P, READY), handle_request(P)),
	!,
	process_requests_loop.
process_requests_loop :-
	log_event("stop").

handle_request(process(PID, FDIN, FDOUT)) :-
	read_message(FDIN, MSG),
	log_event("process %d sends request: %q", [PID, MSG]),
	process_message(MSG, PID, FDOUT).

%% Note: mwrite will not be relayed if there is a process already
%%       blocked for a matching read.
process_message(mwrite(TERM), PID, _) :-
	term_key(TERM, WKEY, RKEY),
	( unblock_process(RKEY, TERM)
	; store_term(WKEY, PID, TERM),
	  relay_message(mwrite(TERM))
	).
process_message(mread(TERM), PID, FDOUT) :-
	term_key(TERM, WKEY, RKEY),
	( recorded(WKEY, TERM, REF)
	-> erase(REF),
	  write_response(FDOUT, PID, TERM),
	  relay_message(mremove(TERM))
	; block_process(RKEY, TERM, PID, FDOUT)
	).
process_message(mreadp(TERM), PID, FDOUT) :-
	term_key(TERM, WKEY, _),
	( recorded(WKEY, TERM)
	-> write_response(FDOUT, PID, TERM)
	; write_response(FDOUT, PID, term_not_found)
	).
process_message(mterminate, PID, _) :-
	log_event("process %d terminated", [PID]),
	disconnect(process(PID, _, _)).
process_message(mremove(TERM), PID, _) :-
	term_key(TERM, WKEY, _),
	( recorded(WKEY, TERM, REF),
	  log_event("process %d removes term: %q", [PID, TERM]),
	  erase(REF),
	  relay_message(mremove(TERM))
	; true
	).
process_message(mregister_relay, PID, FDOUT) :-
	( recorded(relay, PID/_)
	; log_event("process %d registers as relay", [PID]),
	  recordz(relay, PID/FDOUT)
	).
process_message(mderegister_relay, PID, _) :-
	( recorded(relay, PID/_, REF),
	  log_event("process %d deregisters as relay", [PID]),
	  erase(REF)
	; true
	).

term_key(TERM, WKEY, RKEY) :-	
	functor(TERM, N, A),
	atomic_list_concat([N, '&', A], WKEY),
	atomic_list_concat([N, '$', A], RKEY).

write_response(FDOUT, PID, TERM) :-
	log_event("writing response to process %d: %q", [PID, TERM]),
	write_message(FDOUT, TERM).

unblock_process(RKEY, TERM) :-
	recorded(RKEY, blocked(TERM, PID, FDOUT), REF),
	log_event("process %d unblocked by term: %q\n", [PID, TERM]),
	erase(REF),
	write_message(FDOUT, TERM).

store_term(WKEY, PID, TERM) :-
	log_event("process %d stores term: %q", [PID, TERM]),
	recordz(WKEY, TERM).

block_process(RKEY, TERM, PID, FDOUT) :-
	log_event("process %d blocked waiting for term: %q\n", [PID, TERM]),
	recordz(RKEY, blocked(TERM, PID, FDOUT)).

log_event(STR) :-
	log_event(STR, []).

log_event(STR, ARGS) :-
	recorded(boss_verbose, yes),
	!,
	display(user_error, 'BOSS: '),
	fwritef(user_error, STR, ARGS),
	nl(user_error).
log_event(_, _).

relay_message(MSG) :-
	recorded(relay, PID/FDOUT),
	log_event("message relayed to process %d: %q", [PID, MSG]),
	write_message(FDOUT, MSG),
	fail.
relay_message(_).
