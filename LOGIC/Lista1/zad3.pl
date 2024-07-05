is_prime(X) :- 
    \+ (Y is integer(sqrt(X)), /* Negacja, pod Y przypisujemy pierwiastek z X */
        between(2, Y, N), /* Szukamy N-ów od 2 do Y */
        X mod N=:=0 ). /* Takich, że X jest podzielne przez N */

prime(LO, HI, N) :- 
    between(LO, HI, N), 
    N > 1, 
    is_prime(N).