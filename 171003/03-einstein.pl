% prolog

% house(position, color, national, drink, pet, smoke).
houses([
  house(1, _, _, _, _, _),
  house(2, _, _, _, _, _),
  house(3, _, _, _, _, _),
  house(4, _, _, _, _, _),
  house(5, _, _, _, _, _)
]).
locC(S, X, C)   :- member(house(X, C, _, _, _, _), S).
locN(S, X, N)   :- member(house(X, _, N, _, _, _), S).
lives(S, N, C)  :- member(house(_, C, N, _, _, _), S).
keeps(S, N, P)  :- member(house(_, _, N, _, P, _), S).
drinks(S, N, D) :- member(house(_, _, N, D, _, _), S).
smokes(S, N, C) :- member(house(_, _, N, _, _, C), S).

hasNationals(S) :-
  member(house(_, _, englishman, _, _, _), S),
  member(house(_, _, swede     , _, _, _), S),
  member(house(_, _, dane      , _, _, _), S),
  member(house(_, _, norwegian , _, _, _), S),
  member(house(_, _, german    , _, _, _), S).
hasColors(S) :- lives(S, _, red), lives(S, _, blue), lives(S, _, yellow), lives(S, _, white), lives(S, _, green).
hasDrinks(S) :- drinks(S, _, milk), drinks(S, _, bier), drinks(S, _, water), drinks(S, _, tea), drinks(S, _, coffee).
hasPets(S) :- keeps(S, _, dogs), keeps(S, _, birds), keeps(S, _, cats), keeps(S, _, horses), keeps(S, _, fish).
hasSmokes(S) :- smokes(S, _, pallmall), smokes(S, _, dunhill), smokes(S, _, blend), smokes(S, _, bluemasters), smokes(S, _, prince).

neighbors(S, N1, N2) :- locN(S, X, N1), locN(S, Y, N2), Y is X + 1.
neighbors(S, N1, N2) :- locN(S, X, N1), locN(S, Y, N2), X is Y + 1.
%neighbors(S, N1, N2) :- neighbors(S, N2, N1).

cond1(S)  :- lives(S, englishman, red).
cond2(S)  :- keeps(S, swede, dogs).
cond3(S)  :- drinks(S, dane, tea).
cond4(S)  :- locC(S, X, green), locC(S, Y, white), Y is X + 1.
cond5(S)  :- lives(S, N, green), drinks(S, N, coffee).
cond6(S)  :- keeps(S, N, birds), smokes(S, N, pallmall).
cond7(S)  :- lives(S, N, yellow), smokes(S, N, dunhill).
cond8(S)  :- member(house(3, _, _, milk, _, _), S).
cond9(S)  :- locN(S, 1, norwegian).
cond10(S) :- neighbors(S, Smk, Cats), smokes(S, Smk, blend), keeps(S, Cats, cats).
cond11(S) :- smokes(S, N, bluemasters), drinks(S, N, bier).
cond12(S) :- neighbors(S, Hors, Smk), keeps(S, Hors, horses), smokes(S, Smk, dunhill).
cond13(S) :- smokes(S, german, prince).
cond14(S) :- neighbors(S, norwegian, N), lives(S, N, blue).
cond15(S) :- neighbors(S, Smk, Drin), smokes(S, Smk, blend), drinks(S, Drin, water).

sol(S) :- houses(S),
          cond1(S),
          cond2(S),
          cond3(S),
          cond4(S),
          cond5(S),
          cond6(S),
          cond7(S),
          cond8(S),
          cond9(S),
          cond10(S),
          cond11(S),
          cond12(S),
          cond13(S),
          cond14(S),
          cond15(S).
%          hasNationals(S), hasColors(S), hasDrinks(S), hasPets(S), hasSmokes(S).

:- sol(S), keeps(S, N, fish), print(S), writef('\nWho keeps fish: %w\n', [N]).
