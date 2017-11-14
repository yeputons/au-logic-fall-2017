% prolog
:- use_module(library(chr)).

:- chr_constraint east/0, west/0, north/0, south/0.

east, west <=> true.  % true просто выкидывается.
north, south <=> true.
