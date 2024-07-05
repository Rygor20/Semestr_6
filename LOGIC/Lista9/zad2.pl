:- use_module(library(clpfd)).

pack_squares(Widthl*Height,N,X+Y,f(X,N,Y,N)) :-
    X #>= 0,
    X #=< Widthl-N,
    Y #>= 0,
    Y #=< Height-N.

positions_vars([],[]).
positions_vars([X+Y|XYs],[X,Y|Zs]) :-
    positions_vars(XYs,Zs).

kwadraty(Boxes,Width,Height,Zs) :-
    maplist(pack_squares(Width*Height),Boxes,Positions,Constraints),
    disjoint2(Constraints),
    positions_vars(Positions,Zs).

% kwadraty([1,1,1,1,2,2,2,2,3,3], 7, 6, X), label(X).