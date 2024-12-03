:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

%%%%% DCG to read the program consisting of MULs.
% Input can contain 'mul(num, num)' or garbage and mul
input([]) --> eos, !.
input([mul(A, B) | Rest]) --> mul(A, B), input(Rest).
input(Rest) --> [_], input(Rest).

mul(A, B) --> "mul(", integer(A), ",", integer(B),  ")".

%% Execute program consisting of MULs
execute([], 0).
execute([mul(A, B) | Ms], N) :-
		M is A * B,
		execute(Ms, N1),
		N is M + N1.

part1(Ms, Answer) :- execute(Ms, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(input(Muls), Path),
		call(Part, Muls, Answer).
