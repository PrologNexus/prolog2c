%%%% meta-predicates and support code


'$check_callable'('$meta_call'(P, _)) :- foreign_pointer(P), !.
'$check_callable'(X) :- throw(type_error(callable, X)).
