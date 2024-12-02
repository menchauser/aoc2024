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

%% States that N of the Reports are safe (increasing or decreasing).
safe_report(R) :- safe_increase(R).
safe_report(R) :- safe_decrease(R).

input_has_safe_reports(_, [], 0).
input_has_safe_reports(ReportChecker, [R | Rs], N) :-
		call(ReportChecker, R),
		input_has_safe_reports(ReportChecker, Rs, N1),
		N is N1 + 1.
input_has_safe_reports(ReportChecker, [_ | Rs], N) :-
		input_has_safe_reports(ReportChecker, Rs, N).

part1(Reports, Answer) :- input_has_safe_reports(safe_report, Reports, Answer).

%% For part 2, if report is unsafe we should additionally check if it becomes
%% safe by removing any element.

% States that List2 is a List1 with one element removed
fixed_list([L | Ls], [L | Ns]) :- fixed_list(Ls, Ns).
fixed_list([_ | Ls], Ls).

% Now we have to make sure that input has safe reports either for original list
% or for fixed lists.
safe_report_2(R) :- safe_increase(R).
safe_report_2(R) :- safe_decrease(R).
safe_report_2(R) :-
		fixed_list(R, FixedR),
		safe_increase(FixedR).
safe_report_2(R) :-
		fixed_list(R, FixedR),
		safe_decrease(FixedR).
		
part2(Reports, Answer) :- input_has_safe_reports(safe_report_2, Reports, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(reports(Reports), Path),
		call(Part, Reports, Answer).
