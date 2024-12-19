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

%! operator(Type, X, Y, Result).
%  True if combination of X and Y with some operator can produce Result.
operator(_, X, Y, Result) :- Result is X + Y.
operator(_, X, Y, Result) :- Result is X * Y.
operator(ext, X, Y, Result) :-
		atomic_concat(X, Y, C),
		atom_number(C, Result).

%! eval_equation(Type, Result:integer, Numbers:list).
%  True if Numbers could be combined with operators to produce the Result. Type
%  can be 'simple' or 'ext' (to support || operator).
eval_equation(_, R, [R]).
eval_equation(Type, R, [X, Y | Rest]) :-
		operator(Type, X, Y, D1),
		eval_equation(Type, R, [D1 | Rest]).

valid_equation(Type, Result-Numbers) :- eval_equation(Type, Result, Numbers).
