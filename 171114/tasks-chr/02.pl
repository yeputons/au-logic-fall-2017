% prolog
:- use_module(library(chr)).

:- chr_constraint coin1/0, coin2/0, coin50/0, coin20/0, coin10/0.

coin1, coin1 <=> coin2.
coin50, coin50 <=> coin1.
coin20, coin20, coin20, coin20, coin20 <=> coin1.
coin20, coin20, coin10 <=> coin50.
coin10, coin10 <=> coin20.

% Как доказать - непонятно.
