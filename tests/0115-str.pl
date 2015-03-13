main :-
	atomic_list_concat([this, 123, x], this123x),
	atomic_list_concat([abc], abc),
	atomic_list_concat([], ''),
	split_string("//home/foo///bar/", "/", "", ["home", "foo", "bar"]),
	split_string("//home/foo///   bar/", "/", "/ ", ["home", "foo", "bar"]),
	split_string("   word ", "", " ", ["word"]).
	