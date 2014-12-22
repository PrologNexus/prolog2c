main :- foo.
main :- put66).

foo :-
	bar,
	!,
	fail.
foo :- put(65).

bar :- put(48).
bar :- put(49).
