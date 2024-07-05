/*
+--01--+--02--+--03--+
|      |      |      |
04    05     06     07
|      |      |      |
+--08--+--09--+--10--+
|      |      |      |
11    12     13     14
|      |      |      |
+--15--+--16--+--17--+
|      |      |      |
18    19     20     21
|      |      |      |
+--22--+--23--+--24--+

Ułożenie i przyjęta numeracja zapałek
*/

utwórz_małe(Initial, Count, MałyResult) :-
    % Definicje możliwych małych kwadratów
    Kwadrat1 = [1, 4, 5, 8],
    Kwadrat2 = [2, 5, 6, 9],
    Kwadrat3 = [3, 6, 7, 10],
    Kwadrat4 = [8, 11, 12, 15],
    Kwadrat5 = [9, 12, 13, 16],
    Kwadrat6 = [10, 13, 14, 17],
    Kwadrat7 = [15, 18, 19, 22],
    Kwadrat8 = [16, 19, 20, 23],
    Kwadrat9 = [17, 20, 21, 24],
    % Nowy wynik jest albo unią obecnego rozwiązania i kolejnego kwadrata albo nowy kwadrat nie jest dodawany
    ((union(Initial, Kwadrat1, Mały1), Count1 = 1) ; (Mały1 = Initial, Count1 = 0)), 
    ((union(Mały1, Kwadrat2, Mały2), Count2 is Count1 + 1) ; (Mały2 = Mały1, Count2 = Count1)), 
    ((union(Mały2, Kwadrat3, Mały3), Count3 is Count2 + 1) ; (Mały3 = Mały2, Count3 = Count2)), 
    ((union(Mały3, Kwadrat4, Mały4), Count4 is Count3 + 1) ; (Mały4 = Mały3, Count4 = Count3)), 
    ((union(Mały4, Kwadrat5, Mały5), Count5 is Count4 + 1) ; (Mały5 = Mały4, Count5 = Count4)), 
    ((union(Mały5, Kwadrat6, Mały6), Count6 is Count5 + 1) ; (Mały6 = Mały5, Count6 = Count5)), 
    ((union(Mały6, Kwadrat7, Mały7), Count7 is Count6 + 1) ; (Mały7 = Mały6, Count7 = Count6)), 
    ((union(Mały7, Kwadrat8, Mały8), Count8 is Count7 + 1) ; (Mały8 = Mały7, Count8 = Count7)), 
    ((union(Mały8, Kwadrat9, MałyResult), Count is Count8 + 1) ; (MałyResult = Mały8, Count = Count8)).

utwórz_średnie(Initial, Count, ŚredniResult) :-
    % Definicje możliwych średnich kwadratów
    Kwadrat1 = [1, 2, 4, 6, 11, 13, 15, 16], 
    Kwadrat2 = [2, 3, 5, 7, 12, 14, 16, 17], 
    Kwadrat3 = [8, 9, 11, 13, 18, 20, 22, 23], 
    Kwadrat4 = [9, 10, 12, 14, 19, 21, 23, 24], 
    % Nowy wynik jest albo unią obecnego rozwiązania i kolejnego kwadrata albo nowy kwadrat nie jest dodawany
    ((union(Initial, Kwadrat1, Średni1), Count1 = 1) ; (Średni1 = Initial, Count1 = 0)), 
    ((union(Średni1, Kwadrat2, Średni2), Count2 is Count1 + 1) ; (Średni2 = Średni1, Count2 = Count1)), 
    ((union(Średni2, Kwadrat3, Średni3), Count3 is Count2 + 1) ; (Średni3 = Średni2, Count3 = Count2)), 
    ((union(Średni3, Kwadrat4, ŚredniResult), Count is Count3 + 1) ; (ŚredniResult = Średni3, Count = Count3)).

utwórz_duże(Initial, Count, DużyResult) :-
    % Definicje możliwych dużych kwadratów
    Kwadrat1 = [1, 2, 3, 4, 7, 11, 14, 18, 21, 22, 23, 24], 
    % Nowy wynik jest albo unią obecnego rozwiązania i kolejnego kwadrata albo nowy kwadrat nie jest dodawany
    ((union(Initial, Kwadrat1, DużyResult), Count = 1) ; (DużyResult = Initial, Count = 0)).

zapalki(Total, L, M, S) :-
    % Utwórz kolejne możliwe kwadraty
    utwórz_małe([], S, Match1), 
    utwórz_średnie(Match1, M, Match2), 
    utwórz_duże(Match2, L, FinalMatch),
    % Sprawdź czy użyto odpowiedniej ilości zapałek
    length(FinalMatch, Len),
    Total is 24 - Len,
    write("Rozwiazanie:\n"), 
    % Narysuj rozwiązanie
    draw_matches(FinalMatch).

draw_matches(K) :-
    % Sprawdź czy kolejne zapałki obecne są w rozwiązaniu
    % Odpowiednio narysuj zapałkę lub pusty obszar na jej miejscu
    (member(1, K) -> write("+---+") ; write("+   +")),
    (member(2, K) -> write("---+") ; write("   +")),
    (member(3, K) -> write("---+\n") ; write("   +\n")),
    (member(4, K) -> write("|   ") ; write("    ")),
    (member(5, K) -> write("|   ") ; write("    ")),
    (member(6, K) -> write("|   ") ; write("    ")),
    (member(7, K) -> write("|\n") ; write(" \n")),
    (member(8, K) -> write("+---+") ; write("+   +")),
    (member(9, K) -> write("---+") ; write("   +")),
    (member(10, K) -> write("---+\n") ; write("   +\n")),
    (member(11, K) -> write("|   ") ; write("    ")),
    (member(12, K) -> write("|   ") ; write("    ")),
    (member(13, K) -> write("|   ") ; write("    ")),
    (member(14, K) -> write("|\n") ; write(" \n")),
    (member(15, K) -> write("+---+") ; write("+   +")),
    (member(16, K) -> write("---+") ; write("   +")),
    (member(17, K) -> write("---+\n") ; write("   +\n")),
    (member(18, K) -> write("|   ") ; write("    ")),
    (member(19, K) -> write("|   ") ; write("    ")),
    (member(20, K) -> write("|   ") ; write("    ")),
    (member(21, K) -> write("|\n") ; write(" \n")),
    (member(22, K) -> write("+---+") ; write("+   +")),
    (member(23, K) -> write("---+") ; write("   +")),
    (member(24, K) -> write("---+\n") ; write("   +\n")).