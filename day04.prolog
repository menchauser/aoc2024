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


%! chars_has_xmases(+Cs, -N).
%  Input list of chars Cs contains pattern "XMAS" N times when scanning in
%  forward direction.
chars_has_xmases_forward([], 0).
chars_has_xmases_forward(['X', 'M', 'A', 'S' | Rest], N) :-
		chars_has_xmases_forward(Rest, N1),
		N is N1 + 1.
chars_has_xmases_forward([_ | Rest], N) :-
		chars_has_xmases_forward(Rest, N).

%! chars_has_xmases(+Cs, -N).
%  Input list of chars Cs contains pattern "XMAS" N times when scanning in both
%  forward and backward directions.
chars_has_xmases(Input, N) :-
		chars_has_xmases_forward(Input, N1),
		reverse(Input, Reversed),
		chars_has_xmases_forward(Reversed, N2),
		N is N1 + N2.

%! matrix_elements(Matrix, I, J, MaxI, MaxJ, StepI, StepJ, Elements).
%  Elements contains a list of elements of matrix Matrix, starting at
%  coordinates (I, J), going with step of StepI, StepJ until reached the max
%  boundaries.
matrix_elements(Input, I, J, MaxI, MaxJ, StepI, StepJ, [E|Es]) :-
		I =< MaxI, J =< MaxJ,
		nth1(I, Input, Row), nth1(J, Row, E),
		I1 is I + StepI, J1 is J + StepJ,
		matrix_elements(Input, I1, J1, MaxI, MaxJ, StepI, StepJ, Es).
matrix_elements(_, _, _, _, _, _, _, []).

%! matrix_diagonal(Matrix, Diagonal).
%  Diagonal is one of the Matrix's diagonals.
%  Diagonals contains a list in which every vector is a diagonal of a
%  Matrix. All of them together has all elements of Matrix.
matrix_diagonal([], []).
matrix_diagonal(Matrix, Diagonal) :-
		length(Matrix, NumRows),
		nth1(1, Matrix, Row), length(Row, NumCols),
		between(1, NumRows, I),
		matrix_elements(Matrix, I, 1, NumRows, NumCols, 1, 1, Diagonal).

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
		% How many xmases in every diagonal.
		length(Input, Len),
		between(1, Len, Position),
		
		N is HorizontalXmasCount + VerticalXmasCount.
		

