%%%% version number and copyright


prolog_version(3).
prolog_title('?-Prolog').
prolog_copyright('(c)MMXV Felix L. Winkelmann').

show_version_and_exit :-
	prolog_title(T), prolog_copyright(C), prolog_version(V),
	display(T), display(' version '), display(V), display(' - '),
	display(C), nl, halt.
