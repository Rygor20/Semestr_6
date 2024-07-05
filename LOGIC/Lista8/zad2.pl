:- use_module(library(clpfd)).

plecak(Wartosc, Wielkosc, Pojemnosc, Zmienne, Cel):-
    length(Wartosc,L),
    length(Zmienne,L),
    Zmienne ins 0..1,
    scalar_product(Wartosc,Zmienne, #= ,Cel),
    scalar_product(Wielkosc, Zmienne, #=<, Pojemnosc),
    once(labeling([max(Cel)], Zmienne)).

% plecak([10,7,1,3,2],[9,12,2,7,5], 15, X, Cel).