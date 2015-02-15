%%%% version number and copyright


prolog_version(1).
prolog_copyright('?-Prolog (c)MMXV Felix L. Winkelmann').

show_version_and_exit :-
	prolog_copyright(C), prolog_version(V),
	display(C), display(', version '), display(V), nl,
	halt.
