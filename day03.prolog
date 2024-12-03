:- use_module(library(dcg/basics)).
:- use_module(library(pio)).


part1(Prog, Answer) :- execute(Prog, Answer).
part2(Prog, Answer) :- execute_with_conds(enabled, Prog, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(input(Prog), Path),
		call(Part, Prog, Answer).


%%%%% DCG to read the program consisting of MULs.
% Input can contain 'mul(num, num)' or garbage and mul
input([]) --> eos, !.
input([mul(A, B) | Rest]) --> mul(A, B), input(Rest).
input([dont | Rest]) --> "don't()", input(Rest).
input([do | Rest]) --> "do", input(Rest).
input(Rest) --> [_], input(Rest).
mul(A, B) --> "mul(", integer(A), ",", integer(B),  ")".


%! execute(+Prog:list, -Result:int)
%  Execute a program consisting of MULs.
execute([], 0).
execute([mul(A, B) | Rest], N) :-
		M is A * B,
		execute(Rest, N1),
		N is M + N1.
execute([_ | Rest], N) :- execute(Rest, N).


%! execute_with_conds(MulsEnabled, Prog, Result)
%  Execute a program considering conditionals.
execute_with_conds(_, [], 0).
execute_with_conds(enabled, [mul(A, B) | Rest], N) :-
		M is A * B,
		execute_with_conds(enabled, Rest, N1),
		N is M + N1.
execute_with_conds(disabled, [mul(_, _) | Rest], N) :-
		execute_with_conds(disabled, Rest, N).
execute_with_conds(_, [dont | Rest], N) :-
		execute_with_conds(disabled, Rest, N).
execute_with_conds(_, [do | Rest], N) :-
		execute_with_conds(enabled, Rest, N).
