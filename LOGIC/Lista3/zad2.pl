max_sum(List, MaxSum) :-
    max_sum(List, 0, 0, MaxSum).

max_sum([], _, Max, Max). %Przypadek bazowy dla pustej listy


%Analizujemy kolejne elementy z tablicy, skracając listę do analizy w kolejnych wywołaniach, wykona się
%tyle razy, ile elementów ma tablica
max_sum([L|R], CurrentMax, Max, Result) :-
    MaxAfter is CurrentMax + L,
    (MaxAfter > L -> NewCurrent is MaxAfter; NewCurrent is L), %Gdy obecne element jest większy od obecnej sumy, to nie ma sensu brać jej pod uwagę
    (NewCurrent > Max -> NewMax is NewCurrent; NewMax is Max), %Sprawdź znalezione sumy i ustal do przechowywania największą znalezioną
    max_sum(R, NewCurrent, NewMax, Result). %Wywołaj rekurencyjnie dla reszty listy z zaktualizowanymi wartościami