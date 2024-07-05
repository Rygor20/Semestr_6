average(Lista, Result) :-
    length(Lista, N),
    average(Lista, N, 0, Result). %Policz średnią dla Listy o długości N
average([], N, Sum, Result) :-
    Result is Sum / N. %Dla pustej tablicy policz średnią, podziel sumę elementów przez długość (ilość elementów) tablicy
average([H|T], N, Sum, Result) :-
    NewSum is Sum + H, %Dodawaj kolejne elementy tablicy
    average(T, N, NewSum, Result).


wariancja(Lista, Result) :-
    average(Lista, Avg), %Oblicz średniej
    length(Lista, N),
    calc_var(Lista, Avg, N, 0, Result). %Policz wariancję mając długość (ilość elementów) tablicy i średnią elementów


% Obliczanie wariancji przez sumę kwadratów różnic wartości a średnią arytmetyczną
calc_var([], _, Div, Sum, Result) :-
    Result is Sum / Div.
calc_var([H|T], Avg, Div, Sum, Result) :-
    NewSum is Sum + (H - Avg) * (H - Avg),
    calc_var(T, Avg, Div, NewSum, Result).