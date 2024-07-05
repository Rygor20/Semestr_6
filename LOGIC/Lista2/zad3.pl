arc(a, b).
arc(b, a).
arc(b, c).
arc(c, d).



osiągalny(X, X). % X jest osiągalny z samego siebie

osiągalny(X, Y) :-
    osiągalny(X, Y, []).

osiągalny(X, Y, _) :-
    arc(X, Y). % Y jest osiągalny, jeżeli istnieje bezpośrednie połączenie z X

osiągalny(X, Y, Visited) :- % Sprawdź możliwe niebiezpośrednie, nieodwiedzone wcześniej połączenia
    arc(X, Z),
    \+ member(Z, Visited),
    osiągalny(Z, Y, [X|Visited]),
    X \= Y.