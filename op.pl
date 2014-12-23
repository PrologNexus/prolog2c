%%% operator definitions


:- pre_initialization(set_global(operator_table, o[op(700,xfx,'@=<'),
						   op(1100,xfy,';'),
						   op(700,xfx,'@<'),
						   op(1000,xfy,','),
						   op(700,xfx,'is'),
						   op(200,xfx,'**'),
						   op(700,xfx,'@>'),
						   op(400,yfx,'<<'),
						   op(700,xfx,'@>='),
						   op(700,xfx,'\\=='),
						   op(200,xfy,'^'),
						   op(200,fy,'+'),
						   op(500,yfx,'+'),
						   op(400,yfx,'>>'),
						   op(700,xfx,'=='),
						   op(200,fy,'-'),
						   op(500,yfx,'-'),
						   op(1150,fx,'initialization'),
						   op(900,fy,'\\+'),
						   op(700,xfx,'=:='),
						   op(700,xfx,'=\\='),
						   op(500,yfx,'\\/'),
						   op(1200,fx,':-'),
						   op(1200,xfx,':-'),
						   op(700,xfx,'\='),
						   op(200,fy,'\\'),
						      op(500,yfx,'/\\'),
						      op(400,yfx,'xor'),
						      op(700,xfx,'=<'),
						      op(700,xfx,'<'),
						      op(700,xfx,'=..'),
						      op(700,xfx,'='),
						      op(400,yfx,'//'),
						      op(400,yfx,'/'),
						      op(1050,xfy,'->'),
						      op(700,xfx,'>'),
						      op(400,yfx,'*'),
						      op(700,xfx,'>='),
						      op(1200,xfx,'-->'),
						      op(1105,xfy,'|'),
						      op(400,yfx,'\\\\')])).

current_op(P, A, N) :-
	get_global(operator_table, OT),
	member(op(P, A, N), OT).

op(P, A, N) :-
	get_global(operator_table, OT),
	set_global(operator_table, [op(P, A, N)|OT]).
