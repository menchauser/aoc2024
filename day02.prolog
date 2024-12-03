:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).


%%%%% DCG to read list of lists of integers
% Input is a list of reports.
reports([]) --> eos, !.
reports([R | Rs]) --> report(R), reports(Rs).

% Each report is a list of integers on a single line.
report([]) --> eol, !.
report([X | Xs]) --> integer(X), whites, report(Xs).

safe_increase(X, Y) :-
		X < Y, D is (Y - X), between(1, 3, D).

% Pair of numbers is decreased and only with given step boundaries.
safe_decrease(X, Y) :-
		X > Y, D is (X - Y), between(1, 3, D).

% Numbers in report are increasing monotonically in one direction with limited
% step.
safe_report_vals([_], _).
safe_report_vals([X, Y | Xs], increase) :-
		X < Y, D is (Y - X), between(1, 3, D),
		safe_report_vals([Y | Xs], increase).
safe_report_vals([X, Y | Xs], decrease) :-
		X > Y, D is (X - Y), between(1, 3, D),
		safe_report_vals([Y | Xs], decrease).
safe_report(R) :- safe_report_vals(R, _).

% N of the Reports are safe (increasing or decreasing).
input_has_safe_reports(_, [], 0).
input_has_safe_reports(ReportChecker, [R | Rs], N) :-
		call(ReportChecker, R),
		input_has_safe_reports(ReportChecker, Rs, N1),
		N is N1 + 1.
input_has_safe_reports(ReportChecker, [_ | Rs], N) :-
		input_has_safe_reports(ReportChecker, Rs, N).

part1(Reports, Answer) :-
		input_has_safe_reports(safe_report, Reports, Answer).

%% For part 2, if report is unsafe we should additionally check if it becomes
%% safe by removing any element.

% Now we have to make sure that input has safe reports either for original list
% or for fixed lists.
safe_report_2(R) :- safe_report(R).
safe_report_2(R) :-
		select(_, R, FixedR), % Select asserts that FixedR is R without a single element
		safe_report(FixedR).

part2(Reports, Answer) :-
		input_has_safe_reports(safe_report_2, Reports, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(reports(Reports), Path),
		call(Part, Reports, Answer).
