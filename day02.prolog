:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
% :- use_module(library(clpfd)).

%%%%% DCG to read list of lists of integers
% Input is a list of reports.
reports([]) --> eos, !.
reports([R | Rs]) --> report(R), reports(Rs).

% Each report is a list of integers on a single line.
report([]) --> eol, !.
report([X | Xs]) --> integer(X), whites, report(Xs).

% Pair of numbers is increased and only with given step boundaries.
safe_increase(X, Y) :-
		X < Y, D is (Y - X), between(1, 3, D).

% Pair of numbers is decreased and only with given step boundaries.
safe_decrease(X, Y) :-
		X > Y, D is (X - Y), between(1, 3, D).

safe_increase([]).
safe_increase([_]).
safe_increase([X | [Y | Xs]]) :-
		safe_increase(X, Y),
		safe_increase([Y | Xs]).

safe_decrease([]).
safe_decrease([_]).
safe_decrease([X | [Y | Xs]]) :-
		safe_decrease(X, Y),
		safe_decrease([Y | Xs]).

