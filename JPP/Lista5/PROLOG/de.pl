gcd_extended(A, 0, A, 1, 0).
gcd_extended(A, B, G, X, Y) :-
    B \= 0,
    Q is A // B,
    R is A mod B,
    gcd_extended(B, R, G, X1, Y1),
    X is Y1,
    Y is X1 - Q * Y1.

de(A, B, X, Y, G) :-
    gcd_extended(A, B, G, X, Y).