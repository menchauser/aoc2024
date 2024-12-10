:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).

part1(Matrix, Answer) :-
		input_has_xmases(Matrix, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(input(Matrix), Path),
		call(Part, Matrix, Answer).


test_input([[ 1,  2,  3,  4],
						[ 5,  6,  7,  8],
						[ 9, 10, 11, 12],
						[13, 14, 15, 16]]).

% Input is just a list of strings
input([]) --> eos, !.
input([Cs | Ls]) --> string_without("\n", S), { string_chars(S, Cs) }, eol, input(Ls).

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

matrix_top_diagonal(Matrix, Col, D) :-
		% we assume that matrix is square
		length(Matrix, Len),
		matrix_elements(Matrix, 1, Col, Len, Len, 1, 1, D).

matrix_top_rev_diagonal(Matrix, Col, D) :-
		% we assume that matrix is square
		length(Matrix, Len),
		matrix_elements(Matrix, 1, Col, Len, Len, 1, -1, D).

matrix_left_diagonal(Matrix, Row, D) :-
		% we assume that matrix is square
		length(Matrix, Len),
		matrix_elements(Matrix, Row, 1, Len, Len, 1, 1, D).

matrix_right_rev_diagonal(Matrix, Row, D) :-
		% we assume that matrix is square
		length(Matrix, Len),
		matrix_elements(Matrix, Row, Len, Len, Len, 1, -1, D).

%! elements_diagonal(Matrix, Diagonal).
%  Diagonal is one of the Matrix's diagonals.
%  Diagonals contains a list in which every vector is a diagonal of a
%  Matrix. All of them together has all elements of Matrix.
matrix_diagonal([], []).
matrix_diagonal(Matrix, Diagonal) :-
		length(Matrix, NumRows),
		nth1(1, Matrix, Row), length(Row, NumCols),
		between(1, NumRows, I),
		matrix_elements(Matrix, I, 1, NumRows, NumCols, 1, 1, Diagonal).

matrix_has_diagonal_xmases(Matrix, Count) :-
		length(Matrix, RowNum),
		nth1(1, Matrix, FirstRow),
		length(FirstRow, ColNum),
		findall(J, between(1, ColNum, J), Cols),
		findall(I, between(2, RowNum, I), Rows),

		maplist(matrix_top_diagonal(Matrix), Cols, TopDs),
		maplist(matrix_left_diagonal(Matrix), Rows, LeftDs),
		maplist(chars_has_xmases, TopDs, C1), sum_list(C1, TopCount),
		maplist(chars_has_xmases, LeftDs, C2), sum_list(C2, LeftCount),
	  % Now reverse
		maplist(matrix_top_rev_diagonal(Matrix), Cols, TopRevDs),
		maplist(chars_has_xmases, TopRevDs, C3), sum_list(C3, TopRevCount),
		maplist(matrix_right_rev_diagonal(Matrix), Rows, RightRevDs),
		maplist(chars_has_xmases, RightRevDs, C4), sum_list(C4, RightRevCount),
		Count is TopCount + LeftCount + TopRevCount + RightRevCount.

%! input_has_xmases(+Input, -N)
%  Input list of strings Input contains pattern "XMAS" N times (horizontally, vertically, diagonally)
input_has_xmases(Input, N) :- 
		% How many xmases in every row.
		maplist(chars_has_xmases, Input, N1), sum_list(N1, HorizontalXmasCount),
		matrix_has_diagonal_xmases(Input, NormDiagXmasCount),
		% How many xmases in every col.
		transpose(Input, Transposed),
		maplist(chars_has_xmases, Transposed, N2), sum_list(N2, VerticalXmasCount),
		N is HorizontalXmasCount + VerticalXmasCount + NormDiagXmasCount.
