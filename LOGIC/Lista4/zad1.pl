wyrażenie(Lista, Wartość, Wyrażenie) :-
    buduj_wyrażenie(Lista, Wartość, Wyrażenie).

% Predykat pomocniczy do budowy wyrażenia
buduj_wyrażenie([Wartość], Wartość, Wartość) :- !.
buduj_wyrażenie(Lista, Wartość, Wyrażenie) :-
    append(L1, L2, Lista),
    L1 \= [], L2 \= [], % upewniamy się, że listy nie są puste
    buduj_wyrażenie(L1, W1, Wyrażenie1),
    buduj_wyrażenie(L2, W2, Wyrażenie2),
    operator(Operator),
    % upewniamy się, że dzielenie nie będzie przez 0
    \+ (Operator = /, W2 = 0),
    wyrażenie(Wyrażenie1, Operator, Wyrażenie2, Wartość, Wyrażenie).

% Definicje operatorów
operator(+).
operator(-).
operator(*).
operator(/).

% Budowa wyrażenia dla różnych operatorów
wyrażenie(W1, +, W2, W, W1 + W2) :- W is W1 + W2.
wyrażenie(W1, -, W2, W, W1 - W2) :- W is W1 - W2.
wyrażenie(W1, *, W2, W, W1 * W2) :- W is W1 * W2.
wyrażenie(W1, /, W2, W, W1 / W2) :- W2 =\= 0, W is W1 / W2.
