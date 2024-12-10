:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).


test_input([[ 1,  2,  3,  4],
						[ 5,  6,  7,  8],
						[ 9, 10, 11, 12],
						[13, 14, 15, 16]]).

% Input is just a list of strings
input([]) --> eol, !.
input([Cs | Ls]) --> string(S), { string_chars(S, Cs) }, eol, input(Ls).


%! chars_has_xmases(+Cs, -N)
%  Input list of chars Cs contains pattern "XMAS" N times when scanning in
%  forward direction.
chars_has_xmases_forward([], 0).
chars_has_xmases_forward(['X', 'M', 'A', 'S' | Rest], N) :-
		chars_has_xmases_forward(Rest, N1),
		N is N1 + 1.
chars_has_xmases_forward([_ | Rest], N) :-
		chars_has_xmases_forward(Rest, N).

%! chars_has_xmases(+Cs, -N)
%  Input list of chars Cs contains pattern "XMAS" N times when scanning in both
%  forward and backward directions.
chars_has_xmases(Input, N) :-
		chars_has_xmases_forward(Input, N1),
		reverse(Input, Reversed),
		chars_has_xmases_forward(Reversed, N2),
		N is N1 + N2.


%! left_diagonal(Input, N, Diagonal).
%  Select Nth left diagonal of the Input matrix. Left diagonal is stretched from
%  top-left to bottom-right. 
%  Example:
%    1  2  3  4
%    5  6  7  8
%    9 10 11 12
%   13 14 15 16
%  1st left diagonal: 13
%  3rd left diagonal: 5 10 15
left_diagonal([_ | Rest], N, Diagonal) :- fail.
		% we skip (Length - N) rows and then try to work it
		% we need to make sure that value at the coordinates (N, N - i) is 


%! left_diagonal_part(Input, ColNum, Diagonal).
%  Input is a matrix in the form of list of lists. Diagonal is diagonal of
%  elements in that matrix starting from column ColNum in the first row.
left_diagonal_part([], _, []).
left_diagonal_part([Row | _], ColNum, []) :- 
		length(Row, N),
		ColNum > N.
left_diagonal_part([Row | RestRows], ColNum, [D | Ds]) :-
		length(Row, N),
		ColNum =< N,
		nth1(ColNum, Row, D),
		C1 is ColNum + 1,
		left_diagonal_part(RestRows, C1, Ds).

% Now we want to build all possible diagonals and calculate xmases in them.
% Going by each row we 

%! input_has_xmases(+Input, -N)
%  Input list of strings Input contains pattern "XMAS" N times (horizontally, vertically, diagonally)
input_has_xmases(Input, N) :- 
		% How many xmases in every row.
		maplist(chars_has_xmases, Input, N1), sum_list(N1, HorizontalXmasCount),
		% How many xmases in every col.
		transpose(Input, Transposed),
		maplist(chars_has_xmases, Transposed, N2), sum_list(N2, VerticalXmasCount),
		N is HorizontalXmasCount + VerticalXmasCount.
		

