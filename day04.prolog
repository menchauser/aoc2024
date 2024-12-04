:- use_module(library(dcg/basics)).
:- use_module(library(pio)).


% Input is just a list of strings
input([]) --> eol, !.
input([Cs | Ls]) --> string(Cs), eol, input(Ls).


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


%! input_has_xmases(+Input, -N)
%  Input list of strings Input contains pattern "XMAS" N times (horizontally, vertically, diagonally)
input_has_xmases(Input, N) :- fail.
