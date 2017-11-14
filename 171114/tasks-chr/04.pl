% prolog
:- use_module(library(chr)).

:- chr_constraint rev/1, rev2/2, cons/2, nil/0.

rev(X) <=> rev2(X, nil).
% rev2(nil, X) <=> X.  % Переменная не является constraint, поэтому по CHR так писать нельзя, Пролог не ругается и не работает.
rev2(nil, nil) <=> nil.
rev2(nil, cons(X, Y)) <=> cons(X, Y).
rev2(cons(X, Xs), Y) <=> rev2(Xs, cons(X, Y)).
