% prolog

p(foo, bar).
p(bar, foo).

% p(X, foo).

% Есть перестановка из трёх строк.
permVal([
  elem(1, _),
  elem(2, _),
  elem(3, _)
]).
hasAllWords(P) :- member(elem(_, "foo"), P),
                  member(elem(_, "bar"), P),
                  member(elem(_, "baz"), P).
conds(P) :- member(elem(2, "foo"), P),
            member(elem(3, "baz"), P).
sol(P) :- permVal(P), hasAllWords(P), conds(P).

:- sol(P), print(P).
