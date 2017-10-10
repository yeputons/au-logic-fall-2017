% prolog

p(X) :- X = 123.

%even(X) :- 0 == (X mod 2).  % TODO: wtf
%even(X) :- 0 = (X mod 2).  % TODO: wtf
%even(X) :- (X mod 2) is 0.  % TODO: wtf
even(X) :- 0 is (X mod 2).

+/(A) :- A, !, fail.
+/(_).

:- even(6), writef("even(6) is true\n", []).
:- +/(even(5)), writef("+/(even(5)) is true\n", []).

:- p(123).
:- p(_), writef("p(_) is true\n", []).  % exists x: p(x)
:- +/(p(124)).
:- +/(p(_)), writef("+/(p(_)) is true\n", []).  % NOT like this: exists x: !p(x)

:- halt.
