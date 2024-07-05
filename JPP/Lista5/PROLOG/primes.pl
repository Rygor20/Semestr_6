sieve([], []).
sieve([P|Xs], [P|Ys]) :-
    exclude(multiple_of(P), Xs, Zs),
    sieve(Zs, Ys).

multiple_of(P, X) :-
    X mod P =:= 0.

numlist(2, N, List) :- findall(X, between(2, N, X), List).

primes(N, Primes) :-
    N >= 2,
    numlist(2, N, List),
    sieve(List, Primes).