gcd(A, 0, A).
gcd(A, B, G) :-
    B \= 0,
    R is A mod B,
    gcd(B, R, G).

totient(1, 1).
totient(N, T) :-
    N > 1,
    totient(N, N, 0, T).

totient(_, 0, Acc, Acc).
totient(N, I, Acc, T) :-
    I > 0,
    gcd(N, I, 1),
    NewAcc is Acc + 1,
    NewI is I - 1,
    totient(N, NewI, NewAcc, T).
totient(N, I, Acc, T) :-
    I > 0,
    \+ gcd(N, I, 1),
    NewI is I - 1,
    totient(N, NewI, Acc, T).