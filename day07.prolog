:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(strings)).
:- use_module(library(yall)).


exec(Part, Path, Answer) :-
		phrase_from_file(input(Equations), Path),
		call(Part, Equations, Answer).


part1(Equations, Answer) :-
		include(valid_equation, Equations, ValidEquations),
		maplist(arg(1), ValidEquations, ValidResults),
		sum_list(ValidResults, Answer).


input(Equations) --> sequence(equation, Equations).
equation(TestValue-Numbers) -->
		integer(TestValue), ": ", sequence(integer, " ", Numbers), optional_newline.
optional_newline --> "\n" | [].

%! valid_equation(Result:integer, Numbers:list).
%  True if Numbers could be combined with operators to produce the Result.
valid_equation(R, [X, Y]) :- R =:= X * Y.
valid_equation(R, [X, Y]) :- R =:= X + Y.
valid_equation(R, [X, Y, Z | Rest]) :-
		D1 is X * Y,
		valid_equation(R, [D1, Z | Rest]).
valid_equation(R, [X, Y, Z | Rest]) :-
		D1 is X + Y,
		valid_equation(R, [D1, Z| Rest]).

valid_equation(Result-Numbers) :- valid_equation(Result, Numbers).
