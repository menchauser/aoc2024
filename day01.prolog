:- set_prolog_flag(double_quotes, chars).
:- use_module(library(strings)).
:- use_module(library(occurs)).

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
left_number(S, N) :-
		% S contains parts divided by whitespaces
		split_string(S, " ", " ", [Part, _]),
		% The first part is a number
		atom_number(Part, N).

% The second number in string S is N
right_number(S, N) :-
		% S contains two parts divided by whitespaces
		split_string(S, " ", " ", [_, Part]),
		% The second part is a number
		atom_number(Part, N).

% We extract left list of numbers Ns from input list of lines Ls 
left_numbers(Ls, Ns) :-
		maplist(left_number, Ls, Ns).

% We extract right list of numbers Ns from input list of lines Ls 
right_numbers(Ls, Ns) :-
		maplist(right_number, Ls, Ns).
		

% Distance between two numbers
distance(A, B, Distance) :-
		Distance is abs(A - B).

% Input is a big string containing all input data.
part1(Input, Answer) :-
		% Input consists of lines
		string_lines(Input, Lines),
		% Input has first list of numbers, which can be sorted
		% (msort is used instead of sort, to avoid removing duplicates).
		left_numbers(Lines, Ns1), msort(Ns1, Sorted1),
		% Input has second list of numbers, which can be sorted.
		right_numbers(Lines, Ns2), msort(Ns2, Sorted2),
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


%%%%% Part 2 %%%%

% The Score is based on the value of the number X and the number of times it
% appears in the list of numbers Ns.
number_score(X, Ns, Score) :-
		% X appears in Ns Count times.
		occurrences_of_term(X, Ns, Count),
		Score is X * Count.

% L is a list of elements X repeated N times.
repeat(_, 0, []).
repeat(X, N, [X | Xs]) :-
		N > 0,
		N1 is N - 1,
		repeat(X, N1, Xs).

part2(Input, Answer) :-
		% Input consists of lines
		string_lines(Input, Lines),
		% Input consists of two lists of numbers
		left_numbers(Lines, Lefts),
		right_numbers(Lines, Rights),
		% We have a list of Scores which has a score for each element of left list.
		length(Lefts, Len),
		repeat(Rights, Len, RightsN),
		maplist(number_score, Lefts, RightsN, Scores),
		sum_list(Scores, Answer).

exec_part2(Path, Answer) :-
		read_file_to_string(Path, Input, []),
		part2(Input, Answer).
