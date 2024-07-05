is_prime(2).
is_prime(3).
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, F) :-
    N mod F =:= 0.
has_factor(N, F) :-
    F * F < N,
    F2 is F + 2,
    has_factor(N, F2).

smallest_prime_factor(N, F) :-
    smallest_prime_factor(N, F, 2).

smallest_prime_factor(N, N, _) :- is_prime(N).
smallest_prime_factor(N, F, D) :-
    D * D =< N,
    (N mod D =:= 0 -> F = D ; D1 is D + 1, smallest_prime_factor(N, F, D1)).


prime_factors(1, []).
prime_factors(N, [F|Factors]) :-
    N > 1,
    smallest_prime_factor(N, F),
    N1 is N // F,
    prime_factors(N1, Factors).