lista(N, X) :-
    M is N * 2,
    length(X, M), % Upewnij się, że długość jest poprawna
    iter(N, X). % Rozpocznij iterację

iter(1, X) :-
    check(1, X).
iter(N, X) :-
    M is N - 1,
    check(N, X), % Sprawdź kolejne wartości
    iter(M, X). % W kolejnej iteracji sprawdzaj zmienną o jeden mniejszą

check(N, L) :-
    nth0(Ind1, L, N), % Weź indeks wystąpienia N
    nth0(Ind2, L, N), % Weź jeszcze raz indeks wystąpienia N
    Ind1 < Ind2, % Upewnij się, że Ind1 występuje przed Ind2
    Min is N - 1, 
    Max is 2 * Min,
    between(Min, Max, Ind1), % Sprawdź, czy Ind1 jest na dopuszczalnej pozycji
    (Ind2 - Ind1) mod 2 =:= 1. % Indeksy powinny być od siebie w dopuszczalnej odległości