:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(strings)).
:- use_module(library(yall)).


part1(Map, Answer) :-
		map_path(Map, Path),
		sort(Path, DistinctPositions),
		length(DistinctPositions, Answer).


% Input data - just a matrix of characters
input(Map) --> sequence(map_row, "\n", Rows), eos, { exclude(=([]), Rows, Map) }.
map_row(Row) --> string_without("\n", S), { S \= "", string_chars(S, Row) }.

format_map(Map) :-
		maplist([Chars,_]>>format("'~s'~n", [Chars]), Map, _).


% Utilities

%! nth11(?RowIdx, ?ColIdx, ?Matrix, ?Elem)
%  Is true when Elem is at the position (RowIdx, ColIdx) of Matrix. Indices are
%  counted from 1.
nth11(RowIdx, ColIdx, Matrix, Elem) :-
		nth1(RowIdx, Matrix, Row),
		nth1(ColIdx, Row, Elem).

matrix_size(Matrix, Height, Width) :-
		length(Matrix, Height),
		nth1(1, Matrix, Row),
		length(Row, Width).

%! replace(List, Index, Value, NewList)
%  NewList is a List with value at index Index replaced with Value. Index starts
%  from 1.
replace([_ | T], 1, X, [X | T]).
replace([H | T], I, X, [H | R]) :- I > 0, I1 is I - 1, replace(T, I1, X, R), !.
replace(L, _, _, L).

% We want to make some state machine, which builds a next step based on map.

% What we want to achieve is to get next position and state from the current
% ones. To do that we can first take next coords. Then char at that place. Then
% check one of possibilities.

%! step(Map, Pos, Dir, NextPos, NextDir).
%  After a step is made on Map from position Pos given the direction Dir, the
%  new position and direction are defined by NextPos, NextDir.
%  If, after the step, the guard stepped out of the map, its state is changed to
%  '0' (blank).
step(Map, [X1, Y1], S1, [X2, Y2], S2, Dist) :-
		matrix_size(Map, Height, Width),
		% Obtain candidate coordinates for the next position
		next_coords(X1, Y1, S1, X1_, Y1_),
		% Step in that position
		step_(Map, Height, Width, [X1, Y1], [X1_, Y1_], S1, [X2, Y2], S2, Dist).

% "Direction" and "State" are synonyms
%! step_(+Map, +Height, +Width, +OldPos:list, +NewPos:list, +S1, -Pos, -S2, -Distance).
%  Performs calculation of a new state S2 based on old position, candidate new
%  position, state and size of the map. Distance is also calculated: it may be 0
%  if we just rotated.
% Stepping outside of map causes blank state '0'.
step_(  _, Height,     _,            _, [NewX, NewY],  _, [NewX, NewY], '0', 0) :-
		\+ between(1, Height, NewX).
step_(  _,      _, Width,            _, [NewX, NewY],  _, [NewX, NewY], '0', 0) :-
		\+ between(1, Width, NewY).
% If next position is on field or initial state '^' - state remains, position is updated.
step_(Map,     _,      _,            _, [NewX, NewY], S1, [NewX, NewY], S1, 1) :-
		nth11(NewX, NewY, Map, '.').
step_(Map,     _,      _,            _, [NewX, NewY], S1, [NewX, NewY], S1, 1) :-
		nth11(NewX, NewY, Map, '^').
% If next position is obstacle - state is rotated to the right, position remains.
step_(Map, _Height, _Width, [OldX, OldY], [NewX, NewY], S1, [OldX, OldY],  S2, 0) :-
		nth11(NewX, NewY, Map, '#'),
		rotate_state(S1, S2).

rotate_state('>', 'v').
rotate_state('v', '<').
rotate_state('<', '^').
rotate_state('^', '>').

%! next_coords(X1, Y1, Dir, X2, Y2).
%  Determines new position (X2, Y2) based on old position and Direction.
next_coords(X1, Y1, '^', X2, Y1) :- X2 is X1 - 1.
next_coords(X1, Y1, 'v', X2, Y1) :- X2 is X1 + 1.
next_coords(X1, Y1, '>', X1, Y2) :- Y2 is Y1 + 1.
next_coords(X1, Y1, '<', X1, Y2) :- Y2 is Y1 - 1.

%! map_path(+Map:list, -Path:list).
%  Path is a full path from the starting position until going out of the Map
%  bounds. 
map_path(Map, Path) :-
		% Find initial position on the map. Initial direction is always '^'.
		nth11(X, Y, Map, '^'),
		map_path_(Map, '^', [[X, Y]], Path).

%! map_path_(+Map, +Dir:char, PathAcc:list, FinalPath:list) 
map_path_(_  , '0', [_ | Rest], FinalPath) :- reverse(Rest, FinalPath).
map_path_(Map, Dir, [[X, Y] | RestPath], FinalPath) :-
		step(Map, [X, Y], Dir, [NextX, NextY], NextDir, _Dist),
		% DX is abs(X - NextX), DY is abs(Y - NextY),
		% format("(X,Y)=( ~d, ~d), (NX,NY)=( ~d, ~d), DX=~d, DY=~d, Dist=~d~n",
		%			 [X, Y, NextX, NextY, DX, DY, Dist]),
		map_path_(Map, NextDir, [[NextX, NextY], [X, Y] | RestPath], FinalPath).


% To solve part 2 we need a way to detect a loop. To do that we can go into a
% path saving all previous states as (X, Y, Dir). If we are in the same
% coordinates with the same direction: we are in a loop. If we managed to get
% out of the map + we are not in a loop.
% So, let's try copying our map_path, but with loop detector.

% We also have to generate all possible maps with obstacles at Row, Col.

%! map_with_obstacle(Map, Row, Col, NewMap).
%  True is NewMap is a map with obstacle at Row, Col. Solving fails if obstacle
%  cannot be placed there (it already exists at that place).
map_with_obstacle(Map, Row, Col, NewMap) :-
		matrix_size(Map, Rows, Cols),
		between(1, Rows, Row),
		between(1, Cols, Col),
		\+ nth11(Row, Col, Map, '#'),
		nth1(Row, Map, RowData),
		replace(RowData, Col, '#', NewRowData),
		replace(Map, Row, NewRowData, NewMap).

%! map_loops(Map).
%  True if guard loops on the map.
map_loops(Map) :-
		nth11(X, Y, Map, '^'),
		map_loops_(Map, '^', [[X, Y, '^']]).

%! map_loops_(Map, Dir, PathAcc).
%  If we've achieved '0' direction - we are not in loop, so it is false.
map_loops_(_, '0', _) :- !, fail.
map_loops_(Map, Dir, [[X, Y, Dir] | RestPath]) :-
		step(Map, [X, Y], Dir, [NextX, NextY], NextDir, _Dist),
		memberchk([NextX, NextY, NextDir], RestPath).
map_loops_(Map, Dir, [[X, Y, Dir] | RestPath]) :-
		step(Map, [X, Y], Dir, [NextX, NextY], NextDir, _Dist),
		map_loops_(Map, NextDir, [[NextX, NextY, NextDir], [X, Y, Dir] | RestPath]).

map_loops_with_obstacle(Map, Row, Col) :-
		format("Check obstacle at ~d, ~d~n", [Row, Col]),
		map_with_obstacle(Map, Row, Col, NewMap),
		map_loops(NewMap).

% Now find all possible obstacles.
looping_obstacle_count(Map, N) :-
		matrix_size(Map, Rows, Cols),
		findall(
				[Row, Col],
				(
						between(1, Rows, Row),
						between(1, Cols, Col),
						map_with_obstacle(Map, Row, Col, NewMap),
						format("Check obstacle at ~d, ~d~n", [Row, Col]),
						map_loops(NewMap)
				),
				GoodObstacles
		),
		writeln(GoodObstacles),
		length(GoodObstacles, N).
