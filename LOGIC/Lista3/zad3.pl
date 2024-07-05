%Każda parzysta permutacja N+1 elementów jest albo listą przedstawiającą parzystą permutację
%N elementów z (N+1)-szym elementem wstawionym na nieparzystej pozycji na liście, albo listą
%przedstawiającą nieparzystą permutację N elementów z (N+1)-szym elementem wstawionym na parzystej pozycji na liście

even_permutation([], []). %Pusta lista jest permutacją samej siebie, zero swapów też jest parzyste

even_permutation([X|T], Perm) :-
    even_permutation(T, Perm1),
    odd_insert(X, Perm1, Perm).
even_permutation([X|T], Perm) :-
    odd_permutation(T, Perm1),
    even_insert(X, Perm1, Perm).

%Każda nieparzysta permutacja N+1 elementów jest albo listą reprezentującą nieparzystą permutację 
%N elementów z (N+1)-szym elementem wstawionym na nieparzystej pozycji na liście, albo listą 
%przedstawiającą parzystą permutację N elementów z (N+1)-szym elementem wstawionym na parzystej pozycji na liście.

%Tutaj nie ma przypadku bazowego, nie można wykonać swapa, żeby ich ilość była nieparzysta
odd_permutation([X|T], Perm) :-
    odd_permutation(T, Perm1),
    odd_insert(X, Perm1, Perm).
odd_permutation([X|T], Perm) :-
    even_permutation(T, Perm1),
    even_insert(X, Perm1, Perm).


odd_insert(X, InList, [X|InList]).
odd_insert(X, [Y,Z|InList], [Y,Z|OutList]) :-
    odd_insert(X, InList, OutList).

even_insert(X, [Y|InList], [Y,X|InList]).
even_insert(X, [Y,Z|InList], [Y,Z|OutList]) :-
    even_insert(X, InList, OutList).