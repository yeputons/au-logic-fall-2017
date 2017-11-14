% prolog
:- use_module(library(chr)).

:- chr_constraint heat/0, electricity/0, h2/0, o2/0, h2o/0.

heat, h2, h2, o2 <=> h2o, h2o.
electricity, h2o, h2o <=> h2, h2, o2.
