% prolog

max(o, X, X).
max(X, o, X).
max(s(X), s(Y), s(Z)) :- max(X, Y, Z).

le(o, _).
le(s(X), s(Y)) :- le(X, Y).

eq(X, X).

max2(X, Y, Y) :- le(X, Y).
max2(X, Y, X) :- le(Y, X).

max3bad(X, Y, Y) :- le(X, Y).
max3bad(X, _, X).

max3bad2(X, Y, Y) :- le(X, Y), !.
max3bad2(X, _, X).

max4bad(X, Y, Z) :- le(X, Y), eq(Y, Z).
max4bad(X, _, X).

max4(X, Y, Z) :- le(X, Y), !, eq(Y, Z).
max4(X, _, X).

:- max(s(s(s(o))), s(s(s(s(s(s(o)))))), A), writef("%w\n", [A]).
:- max(s(s(s(s(s(s(o)))))), s(s(s(o))), A), writef("%w\n", [A]).
:- max2(s(s(s(o))), s(s(s(s(s(s(o)))))), A), writef("%w\n", [A]).
:- max2(s(s(s(o))), s(s(s(s(s(s(o)))))), A), writef("%w\n", [A]).
:- max3bad(s(s(s(s(s(s(o)))))), s(s(s(o))), A), writef("%w\n", [A]).
:- max3bad(s(s(s(s(s(s(o)))))), s(s(s(o))), A), writef("%w\n", [A]).
:- max4bad(s(s(s(s(s(s(o)))))), s(s(s(o))), A), writef("%w\n", [A]).
:- max4bad(s(s(s(s(s(s(o)))))), s(s(s(o))), A), writef("%w\n", [A]).
:- max(o, s(o), o), writef("bad max\n").
:- max(s(o), o, o), writef("bad max\n").
:- max2(o, s(o), o), writef("bad max2\n").
:- max2(s(o), o, o), writef("bad max2\n").
:- max3bad(s(o), o, o), writef("bad max3bad\n").
:- max3bad(o, s(o), o), writef("bad max3bad\n").
:- max3bad2(s(o), o, o), writef("bad max3bad2\n").
:- max3bad2(o, s(o), o), writef("bad max3bad2\n").
:- max4bad(s(o), o, o), writef("bad max4bad\n").
:- max4bad(o, s(o), o), writef("bad max4bad\n").
:- max4(s(o), o, o), writef("bad max4\n").
:- max4(o, s(o), o), writef("bad max4\n").

maxn(X, Y, Y) :- X =< Y.
maxn(X, Y, X) :- X >= Y.

:- maxn(3, 6, A), maxn(6, 3, B), writef("%w %w\n", [A, B]).

:- halt.
