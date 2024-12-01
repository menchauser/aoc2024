:- set_prolog_flag(double_quotes, chars).
:- use_module(library(strings)).

% How to test:
%   test_input(Input), part1(Input, Answer).
test_input(X) :-
		X = {|string||
				 |3   4
				 |4   3
				 |2   5
				 |1   3
				 |3   9
				 |3   3|}.

% The first number in string S is N
first_number(S, N) :-
		% S contains parts divided by whitespaces
		split_string(S, " ", " ", [Part, _]),
		% The first part is a number
		atom_number(Part, N).

% The first number in string S is N
second_number(S, N) :-
		% S contains two parts divided by whitespaces
		split_string(S, " ", " ", [_, Part]),
		% The second part is a number
		atom_number(Part, N).

% Distance between two numbers
distance(A, B, Distance) :-
		Distance is abs(A - B).

% Input is a big string containing all input data.
part1(Input, Answer) :-
		% Split input into lines.
		string_lines(Input, Lines),
		% Input has first list of numbers, which can be sorted
		% (msort is used instead of sort, to avoid removing duplicates).
		maplist(first_number, Lines, Ns1), msort(Ns1, Sorted1),
		% Input has second list of numbers, which can be sorted.
		maplist(second_number, Lines, Ns2), msort(Ns2, Sorted2),
		% We calculate the distance between paired list elements (smallest with
		% smallest, etc).
		maplist(distance, Sorted1, Sorted2, Distances),
		% The total distance is just a sum of each pair distances.
		sum_list(Distances, Answer).

% Load input data from the file located by Path and calculate the Answer.
exec_part1(Path, Answer) :-
		% Read whole file.
		read_file_to_string(Path, Input, []),
		% Run Part 1.
		part1(Input, Answer).

