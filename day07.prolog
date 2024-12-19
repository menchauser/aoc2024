:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(pio)).


exec(Part, Path, Answer) :-
		phrase_from_file(input(Equations), Path),
		call(Part, Equations, Answer).


part1(Equations, Answer) :-
		include(valid_equation(simple), Equations, ValidEquations),
		maplist(arg(1), ValidEquations, ValidResults),
		sum_list(ValidResults, Answer).

part2(Equations, Answer) :-
		include(valid_equation(ext), Equations, ValidEquations),
		maplist(arg(1), ValidEquations, ValidResults),
		sum_list(ValidResults, Answer).


input(Equations) --> sequence(equation, Equations).
equation(TestValue-Numbers) -->
		integer(TestValue), ": ", sequence(integer, " ", Numbers), optional_newline.
optional_newline --> "\n" | [].

%! valid_equation(Type, Result:integer, Numbers:list).
%  True if Numbers could be combined with operators to produce the Result. Type
%  can be 'simple' or 'ext' (to support || operator).
valid_equation(_, R, [R]).
valid_equation(Type, R, [X, Y | Rest]) :-
		D1 is X * Y,
		valid_equation(Type, R, [D1 | Rest]).
valid_equation(Type, R, [X, Y | Rest]) :-
		D1 is X + Y,
		valid_equation(Type, R, [D1 | Rest]).
valid_equation(ext, R, [X, Y | Rest]) :-
		number_chars(X, C1),
		number_chars(Y, C2),
		append(C1, C2, C3),
		number_chars(D1, C3),
		valid_equation(ext, R, [D1 | Rest]).

valid_equation(Type, Result-Numbers) :- valid_equation(Type, Result, Numbers).
