% prolog

add(o, X, X).
add(s(X), Y, Z) :- add(X, s(Y), Z).

sub(X, Y, Z) :- add(Y, Z, X).

mul(o, _, o).
mul(s(X), Y, Z) :- mul(X, Y, Z0), add(Y, Z0, Z).

div(X, Y, Z) :- mul(Y, Z, X).

le(o, _).
le(s(X), s(Y)) :- le(X, Y).

ge(Y, X) :- le(X, Y).

eq(X, X).

divider(N, X) :- mul(X, _, N).

fact(o, s(o)).
fact(s(X), FsX) :- fact(X, FX), mul(s(X), FX, FsX).

npeano(0, o).
npeano(X, s(Y)) :- npeano(Z, Y), X is Z + 1.

:- writef("Usage: -q -s 01-peano.pl -t fact_1,fact_2\n").
fact_1 :- fact(X, Y), npeano(5, X), npeano(Z, Y), writef('Fact(5)=%w\n', [Z]).
fact_2  :- fact(X, Y), npeano(120, Y), npeano(Z, X), writef('Fact(%w)=120\n', [Z]).
