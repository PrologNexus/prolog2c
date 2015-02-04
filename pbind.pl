%%% bindings generator
%
%
% Currently understands the following:
%
% TYPE IDENTIFIER [("[" (NUMBER) "]") ...]
% TYPE IDENTIFIER "(" (TYPE [IDENTIFIER [("[" (NUMBER) "]") ...]]) "," ... ")"
%
% TYPE = ["const"] BASETYPE ("const" | "*") ...


%% parsing of definitions

parse_definition(NAME, REALNAME, RTYPE, ARGTYPES) -->
	ws, result_type(RTYPE1), ws, entity_name(NAME, REALNAME),
	ws, parse_suffix(RTYPE1, RTYPE, ARGTYPES), ws, (";"; "").

parse_suffix(RTYPE, FINALRTYPE, ARGTYPES) --> indices(RTYPE, FINALRTYPE).
parse_suffix(RTYPE, RTYPE, ARGTYPES) --> arguments(ARGTYPES).
parse_suffix(RTYPE, RTYPE, none) --> [].
	     
%%XXX handle comments
ws([C|R], OUT) :- memberchk(C, [32, 10, 13, 9]), !, ws(R, OUT).
ws(IN, IN).

eof(IN) :- ws(IN, []).

result_type(TYPE) --> type(TYPE).

argument_type(TYPE) -->	type(TYPE).

identifier(ID, [C|IN], OUT) :-
	(uppercase(C); lowercase(C); C == 95),
	identifier_chars(IN, OUT, IR),
	name(ID, [C|IR]).

identifier_chars([C|IN], OUT, [C|MORE]) :-
	(uppercase(C); lowercase(C); digit(C); C == 95),
	identifier_chars(IN, OUT, MORE).
identifier_chars(IN, IN, "").

numeric_literal(NUM, [C|IN], OUT) :-
	digit(C),
	number_chars(IN, OUT, IR),
	number_codes(NUM, [C|IR]).

number_chars([C|IN], OUT, [C|MORE]) :-
	digit(C), number_chars(IN, OUT, MORE).
number_chars(IN, IN, "").

entity_name(NAME, NAME) -->
	%%XXX allow renaming of the form "/* => IDENTIFIER */"
	identifier(NAME).

indices(RTYPE, FINALRTYPE) -->
	"[", ws, (numeric_literal(_); ""), ws, "]",
	(indices(pointer(RTYPE), FINALRTYPE); {FINALRTYPE = RTYPE}).

arguments([]) --> "(", ws, ")".
arguments(ARGTYPES) --> "(", ws, argument_type_list(ARGTYPES), ws, ")".

argument_type_list([TYPE|MORE]) -->
	argument_type(TYPE1), ws,
	(identifier(_), argument_type_suffix(TYPE1, TYPE); {TYPE1 = TYPE}),
	ws, ",", ws, argument_type_list(MORE).
argument_type_list([TYPE]) --> argument_type(TYPE).

argument_type_suffix(T1, T2) --> indices(T1, T2).
argument_type_suffix(T, T) --> [].

type(TYPE) -->
	type_prefixes(PREFIXED, BASETYPE), ws, identifier(BASETYPE), ws, type_qualifiers(PREFIXED, TYPE).

%%XXX unsigned
type_prefixes(const(TYPE), VAR) -->
	"const", ws, type_prefixes(TYPE, VAR).
type_prefixes(VAR, VAR) --> [].

type_qualifiers(BASETYPE, TYPE) -->
	"*", ws, type_qualifiers(pointer(BASETYPE), TYPE).
type_qualifiers(BASETYPE, TYPE) -->
	"const", ws, type_qualifiers(const(BASETYPE), TYPE).
type_qualifiers(TYPE, TYPE) --> [].


%% maintain stored definition info

store_definition(NAME, REALNAME, RESULTTYPE, none) :-
	write(variable(REALNAME, RESULTTYPE)), nl,
	recordz(definitions, variable(NAME, REALNAME, RESULTTYPE)).
store_definition(NAME, REALNAME, RESULTTYPE, ARGTYPES) :-
	write(function(REALNAME, RESULTTYPE, ARGTYPES)), nl,
	recordz(definitions, function(NAME, REALNAME, RESULTTYPE, ARGTYPES)).	


%% generate file with primitives

generate_header_file(FNAME) :-
	tell(FNAME), generate_primitives, told.

generate_primitives :-
	recorded(definitions, variable(NAME, REALNAME, RTYPE)),
	variable_primitive(NAME, REALNAME, RTYPE),
	fail.
generate_primitives :-
	recorded(definitions, function(NAME, REALNAME, RTYPE, ARGTYPES)),
	function_primitive(NAME, REALNAME, RTYPE, ARGTYPES),
	fail.
generate_primitives.

variable_primitive(NAME, REALNAME, RTYPE) :-
	gen('\nPRIMITIVE(v_', REALNAME, ',X x){\nreturn unify('),
	p_value(RTYPE, NAME),
	gen(',x_);}\n'),
	%%XXX no setter if const
	gen('\nPRIMITIVE(set_v_', REALNAME, ',X x){\n', NAME, '='),
	c_value(RTYPE, 'x'),
	gen(';\nreturn 1;}\n'),	!.

function_primitive(NAME, REALNAME, void, []) :-
	gen('\nstatic inline int f_', REALNAME, '(CHOICE_POINT *C0){\n'),
	gen(NAME, '();\nreturn 1;}\n'), !.
function_primitive(NAME, REALNAME, RTYPE, []) :-
	gen('\nPRIMITIVE(f_', REALNAME, ',X r){\n'),
	gen_call(RTYPE, NAME, [], 'x'),
	gen('\nreturn unify('),
	p_value(RTYPE, 'x'),
	gen(',r);}\n'), !.
function_primitive(NAME, REALNAME, RTYPE, ARGTYPES) :-
	gen('\nPRIMITIVE(f_', REALNAME, ',X r'),
	length(ARGTYPES, ARGC),
	forall(between(1, ARGC, I), gen(',X x', I)),
	gen('){\n'),
	gen_call(RTYPE, NAME, ARGTYPES, 'x'),
	gen('\nreturn unify('),
	p_value(RTYPE, 'x'),
	gen(',r);}\n'), !.

gen_call(RTYPE, NAME, ARGTYPES, RESULT) :-
	gen(RTYPE, ' ', RESULT, '=', NAME),
	gen('('),
	gen_call(1, ARGTYPES),
	gen(');').

gen_call(_, []).
gen_call(I, [ATYPE]) :-
	gen_call_arg(I, ATYPE).
gen_call(I, [ATYPE|R]) :-
	gen_call_arg(I, ATYPE),
	gen(','),
	I2 is I + 1,
	gen_call(I2, R).

gen_call_arg(I, TYPE) :-
	number_codes(I, IL),
	name(ARG, [120|IL]),
	c_value(TYPE, ARG).


%% generate file with wrapper predicates

generate_wrapper_file(FNAME, HNAME) :-
	tell(FNAME),
	name(HFILE, HNAME),
	gen(':- verbatim(\'#include "', HFILE, '"\').\n'),
	generate_wrappers,
	told.

generate_wrappers :-
	recorded(definitions, variable(_, REALNAME, _)),
	variable_wrapper(REALNAME),
	fail.
generate_wrappers :-
	recorded(definitions, function(_, REALNAME, _, ARGTYPES)),
	function_wrapper(REALNAME, ARGTYPES),
	fail.
generate_wrappers.

variable_wrapper(NAME) :-
	gen(':- determinate([', NAME, '/1, set_', NAME, '/1]).\n'),
	gen(NAME, '(X) :- foreign_call(v_', NAME, '(X)).\n'),
	%%XXX no setter if const
	gen('set_', NAME, '(X) :- foreign_call(set_v_', NAME, '(X)).\n'),
	!.

function_wrapper(NAME, ARGTYPES) :-
	findall(_, member(_, ARGTYPES), VARS),
	length(VARS, N),
	gen(':- determinate(', NAME, '/', N, ').\n'),
	gen(NAME, '('),
	gen_list(VARS),
	gen(') :- foreign_call(f_', NAME, '('),
	gen_list(VARS),
	gen(')).\n'),
	!.


%% value conversion from and to C

p_value(const(T), REF) :- p_value(T, REF).
p_value(pointer(char), REF) :- gen('CATOM(', REF, ')'). %XXX ptr to const char
p_value(ITYPE, REF) :-
	memberchk(ITYPE, [char, int, long, short]),
	gen('word_to_fixnum(', REF, ')').
p_value(FTYPE, REF) :-
	memberchk(FTYPE, [float, double]),
	gen('FLONUM(', REF, ')').
p_value(TYPE, _) :-
	error('no C->Prolog conversion for type', TYPE).

c_value(const(T), REF) :- c_value(T, REF).
c_value(pointer(char), REF) :-gen('((const char *)objdata(', REF, '))'). %XXX s.a.
c_value(ITYPE, REF) :-
	memberchk(ITYPE, [char, int, long, short]),
	gen('(is_FIXNUM(', REF, ')?fixnum_to_word(', REF),
	gen('):flonum_to_float(check_type_FLONUM(', REF, ')))').
c_value(FTYPE, REF) :-
	memberchk(FTYPE, [float, double]),
	gen('(is_FIXNUM(', REF, ')?fixnum_to_float(', REF),
	gen('):flonum_to_float(check_type_FLONUM(', REF, ')))').
c_value(TYPE, _) :-
	error('no Prolog->C conversion for type', TYPE).


%% utilities

error(MSG, ARG) :-
	current_error_output(OUT), tell(OUT),
	gen(MSG, ': '), writeq(ARG), nl,
	halt(1).

gen(T) :- display(T).
gen(T1, T2) :- gen(T1), gen(T2).
gen(T1, T2, T3) :- gen(T1, T2), gen(T3).
gen(T1, T2, T3, T4) :- gen(T1, T2, T3), gen(T4).
gen(T1, T2, T3, T4, T5) :- gen(T1, T2, T3, T4), gen(T5).

gen_list([]) :- !.
gen_list([X]) :- gen(X), !.
gen_list([X|R]) :- gen(X, ','), gen_list(R).

uppercase(C) :- C >= 65, C =< 90.
lowercase(C) :- C >= 97, C =< 122.
digit(C) :- C >= 48, C =< 57.

basename(FNAME, BNAME) :-
	(atom(FNAME) -> name(FNAME, LST); LST = FNAME),
	append(BNAME, [46|_], LST), !.

read_all(LST) :- get0(C), read_all(C, LST).

read_all(-1, []) :- !.
read_all(C, [C|R]) :- get0(C2), read_all(C2, R).

take_list([], _, []).
take_list(_, 0, []).
take_list([X|R], N, [X|R2]) :- N2 is N - 1, take_list(R, N2, R2).


%% toplevel

parse_arguments([]).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments([FILENAME|MORE]) :-
	(\+recorded(source_file, _); usage(1)),
	recordz(source_file, FILENAME),
	parse_arguments(MORE).

usage(CODE) :-
	display('usage: pbind [FILENAME]\n'),
	halt(CODE).

process_input_file(user) :-
	!, read_all(INPUT), process_input(INPUT).
process_input_file(FILENAME) :-
	see(FILENAME), read_all(INPUT), seen,
	process_input(INPUT).

process_input(INPUT) :-
	parse_definition(N, RN, RT, AT, INPUT, REST),
	store_definition(N, RN, RT, AT),
	!, process_input(REST).
process_input(INPUT) :-
	eof(INPUT), !.
process_input(INPUT) :-
	take_list(INPUT, 100, LST),
	append(LST, "...", LST2),
	name(STR, LST2),
	error('invalid syntax', STR).

main :-
	command_line_arguments(ARGS),
	parse_arguments(ARGS),
	(recorded(source_file, SOURCEFILE); SOURCEFILE = user),
	process_input_file(SOURCEFILE),
	basename(SOURCEFILE, BASENAME),
	append(BASENAME, ".h", HNAME),
	generate_header_file(HNAME),
	append(BASENAME, ".pl", PLNAME),
	generate_wrapper_file(PLNAME, HNAME).
