% ?- open('ex1.prog', read, X), scanner(X, Y), close(X), write(Y).

% Słowa kluczowe
klucz(read).
klucz(write).
klucz(if).
klucz(then).
klucz(else).
klucz(fi).
klucz(while).
klucz(do).
klucz(od).
klucz(and).
klucz(or).
klucz(mod).


% Separatory
separator(';').
separator('+').
separator('-').
separator('*').
separator('/').
separator('(').
separator(')').
separator('<').
separator('>').
separator('=<').
separator('>=').
separator(':=').
separator('=').
separator('/=').

% Białe znaki
biały_znak('\t'). % Tabulator
biały_znak(' ').  % Spacja
biały_znak('\n'). % Nowa linia

% Predykat sprawdzający, czy dany znak jest cyfrą
liczba_naturalna([H|T]) :- char_type(H, digit), liczba_naturalna(T).
liczba_naturalna([]) :- !.

% Predykat sprawdzający, czy dany znak jest dużą literą
identyfikator([H|T]) :- char_type(H, upper), identyfikator(T).
identyfikator([]) :- !.

% Predykat czytający następny token
czytaj_następny('end_of_file', _, [], []) :- !.

czytaj_następny(Znak, Znak2, Lista, Wynik) :-
    % Jeśli następny znak jest białym znakiem, pomijamy go i czytamy dalej
    biały_znak(Znak2),
    get_char(Znak3),
    czytaj_następny(Znak2, Znak3, Lista2, Wynik2),
    redukuj(Lista2, Wynik2, Wynik),
    Lista = [Znak].

czytaj_następny(Znak, Znak2, Lista, [WynikGłowy | WynikOgon]) :-
    % Jeśli następny znak jest separatorem, czytamy dalej
    separator(Znak2),
    get_char(Znak3),
    czytaj_następny(Znak2, Znak3, Lista2, Wynik2),
    (
        % Jeśli możliwe jest złożenie separatora z kolejnym znakiem, rozpoznajemy go jako jeden token
        (   atom_chars(Word, [Znak, Znak2]), separator(Word), redukuj(Lista2, Wynik2, WynikOgon), WynikGłowy = separator(Word), Lista = [] );
        % W przeciwnym razie traktujemy go jako osobny token
        (   redukuj(Lista2, Wynik2, WynikOgon), WynikGłowy = separator(Znak2), Lista = [Znak] )
    ).

czytaj_następny(Znak, Znak2, [Znak | Lista], Wynik) :-
    % Jeśli następny znak nie jest białym znakiem ani separatorem, jest to początek nowego tokena
    get_char(Znak3),
    czytaj_następny(Znak2, Znak3, Lista, Wynik).

% Redukcja listy do listy tokenów
redukuj([_|[]], X, X).
redukuj([_|Ogon], Wejście, Wyjście) :- 
    atom_token(Ogon, Token),
    Wyjście = [Token|Wejście].

% Rozpoznawanie tokenów
atom_token(Lista, Wynik) :-
    (   identyfikator(Lista), atom_chars(Tok, Lista), Wynik = id(Tok) ); % Identyfikator
    (   liczba_naturalna(Lista), atom_chars(Tok, Lista), Wynik = int(Tok) ); % Liczba naturalna
    (   atom_chars(Tok, Lista), klucz(Tok), Wynik = klucz(Tok) ). % Słowo kluczowe

% Główny predykat skanera
scanner(X, Y) :-
    set_input(X),
    get_char(Znak),
    czytaj_następny(' ', Znak, Lista, Wynik),
    redukuj(Lista, Wynik, Y).
