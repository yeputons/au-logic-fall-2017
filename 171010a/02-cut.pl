% prolog
f(a).
f(b).
g(b).
h(a).
h(b).
any(_).

foo1(X) :- f(X), g(X).
foo2(X) :- f(X), !, g(X).

bar1(X) :- foo2(X).
bar2(X) :- f(X), foo2(X).
bar3(X) :- h(X), foo2(X).
bar4(X) :- any(X), foo2(X).
bar5(X) :- foo2(X), f(X).

:- foo1(b).
:- foo2(b).
:- bar1(b).
:- bar2(b).
:- bar3(b).
:- bar4(b).
:- bar5(b).

:- foo1(_), writef("OK\n", []).
:- foo2(_), writef("OK\n", []). % obviously fails as it tries X=a only.
:- bar1(_), writef("OK\n", []). % fails because there is no branching before entering foo2.
:- bar2(_), writef("OK\n", []).
:- bar3(_), writef("OK\n", []).
:- bar4(_), writef("OK\n", []). % fails for the same reason: any() is not a branching point.
:- bar5(_), writef("OK\n", []). % fails because we go into foo2(X) first, not into f(X).

:- halt.
