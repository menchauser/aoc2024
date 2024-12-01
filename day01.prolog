:- set_prolog_flag(double_quotes, chars).
:- use_module(library(strings)).
:- use_module(library(occurs)).
:- use_module(library(dcg/basics)).

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


%%%%% Original string parser
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
		
%%%%% Alternative approach: use DCG for parsing
% Input is two lists of integers, collected line by line
input([], []) --> eol, !.
input([A | As], [B | Bs]) --> input_line(A, B), input(As, Bs).
% Each line contains elements from left and right list separated by whitespace
input_line(A, B) --> integer(A), whites, integer(B), eol.


%%%%% Part 1
% Distance between two numbers
distance(A, B, Distance) :-
		Distance is abs(A - B).

% Input is a big string containing all input data.
part1(Lefts, Rights, Answer) :-
		% Input lists can be sorted (using msort instead of sort, to avoid removing
		% duplicates).
		msort(Lefts, Sorted1), msort(Rights, Sorted2),
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


%%%%% Part 2

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

% Calculate the answer for list of Lefts and Rights
part2(Lefts, Rights, Answer) :-
		% We have a list of Scores which has a score for each element of left list.
		length(Lefts, Len),
		repeat(Rights, Len, RightsN),
		maplist(number_score, Lefts, RightsN, Scores),
		sum_list(Scores, Answer).

% Executors
exec(part1, Path, Answer) :-
		phrase_from_file(input(Lefts, Rights), Path),
		part1(Lefts, Rights, Answer).
										 
exec(part2, Path, Answer) :-
		phrase_from_file(input(Lefts, Rights), Path),
		part2(Lefts, Rights, Answer).


