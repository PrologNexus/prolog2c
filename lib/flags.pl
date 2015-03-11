%%% prolog flags


current_prolog_flag(version, 4).

current_prolog_flag(prolog_title, '?-Prolog').
current_prolog_flag(prolog_copyright, '(c)MMXV Felix L. Winkelmann').
current_prolog_flag(bounded, true).
current_prolog_flag(max_arity, 9999). % arbitrary
current_prolog_flag(integer_rounding_function, toward_zero). %XXX correct?
current_prolog_flag(max_integer, N) :- foreign_call(fixnum_bounds(_, N)).
current_prolog_flag(min_integer, N) :- foreign_call(fixnum_bounds(N, _)).
current_prolog_flag(unknown, error).
