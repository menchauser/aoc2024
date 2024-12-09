:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(strings)).
:- use_module(library(yall)).


% Input data - just a matrix of characters
input(Map) --> sequence(map_row, "\n", Map).
map_row(Row) --> string_without("\n", S), { string_chars(S, Row) }.

format_map(Map) :-
		maplist([Chars,_]>>format("~s~n", [Chars]), Map, _).


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

% We want to make some state machine, which builds a next step based on map.

% What we want to achieve is to get next position and state from the current
% ones. To do that we can first take next coords. Then char at that place. Then
% check one of possibilities.

%! step(Map, Pos, Dir, NextPos, NextDir).
%  After a step is made on Map from position Pos given the direction Dir, the
%  new position and direction are defined by NextPos, NextDir.
%  If, after the step, the guard stepped out of the map, its state is changed to
%  '0' (blank).
step(Map, [X1, Y1], S1, [X2, Y2], S2) :-
		matrix_size(Map, Height, Width),
		% Obtain candidate coordinates for the next position
		next_coords(X1, Y1, S1, X1_, Y1_),
		% Step in that position
		step_(Map, Height, Width, [X1, Y1], [X1_, Y1_], S1, [X2, Y2], S2).

% "Direction" and "State" are synonyms
%! step_(+Map, +Height, +Width, +OldPos:list, +NewPos:list, +S1, -Pos, -S2).
%  Performs calculation of a new state S2 based on old position, candidate new
%  position, state and size of the map.
% Stepping outside of map causes blank state '0'.
step_(  _, Height,     _,            _, [NewX, NewY],  _, [NewX, NewY], '0') :-
		\+ between(1, Height, NewX).
step_(  _,      _, Width,            _, [NewX, NewY],  _, [NewX, NewY], '0') :-
		\+ between(1, Width, NewY).
% If next position is on field or initial state '^' - state remains, position is updated.
step_(Map,     _,      _,            _, [NewX, NewY], S1, [NewX, NewY], S1) :-
		nth11(NewX, NewY, Map, '.').
step_(Map,     _,      _,            _, [NewX, NewY], S1, [NewX, NewY], S1) :-
		nth11(NewX, NewY, Map, '^').
% If next position is obstacle - state is rotated to the right, position remains.
step_(Map,     _,      _, [OldX, OldY], [NewX, NewY], S1, [OldX, OldY],  S2) :-
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

%! map_path_length(+Map, -Length).
map_path_length(Map, Length) :-
		% Find initial position on the map. We expect initial duration to always be '^'.
		nth11(X, Y, Map, '^'),
		map_path_length_(Map, X, Y, '^', Length).

%! map_path_length_(+Map, +Height, +Width, +X, +Y, +Dir, -Length) 
map_path_length_(_  , _, _, '0',      _).
map_path_length_(Map, X, Y, Dir, Length) :-
		step(Map, [X, Y], Dir, [NextX, NextY], NextDir),
		map_path_length_(Map, NextX, NextY, NextDir, L1),
		Length is L1 + 1.
