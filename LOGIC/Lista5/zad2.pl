% hetmany(5, X), board(X).


% Predykat hetmany/2
hetmany(N, Ustawienie) :-
  numlist(1, N, Lista),
  permutation(Lista, Ustawienie),
  dobre_ustawienie(Ustawienie).

% Predykat dobre_ustawienie/1 sprawdzający, czy ustawienie hetmanów jest poprawne
dobre_ustawienie(Ustawienie) :-
  \+ zle_ustawienie(Ustawienie).

% Predykat zle_ustawienie/1 sprawdzający, czy ustawienie hetmanów jest złe
zle_ustawienie(Ustawienie) :-
  append(_, [Wi | L1], Ustawienie),
  append(L2, [Wj | _], L1),
  length(L2, K),
  abs(Wi - Wj) =:= K + 1.

% Predykat board/1 rysujący szachownicę z hetmanami
board(Ustawienie) :-
  length(Ustawienie, Dlugosc),
  rysuj_poziom(Dlugosc, Dlugosc, Ustawienie),
  rysuj_gorna_granice(Dlugosc).

% Predykat rysuj_poziom/3 rysujący poziom planszy
rysuj_poziom(Poziom, Szerokosc, Ustawienie) :-
  Poziom > 0,
  Poziom mod 2 =:= 0,
  rysuj_gorna_granice(Szerokosc),
  rysuj_pola_biale(Poziom, Ustawienie),
  rysuj_pola_biale(Poziom, Ustawienie),
  Mniejszy_Poziom is Poziom - 1,
  rysuj_poziom(Mniejszy_Poziom, Szerokosc, Ustawienie).

rysuj_poziom(Poziom, Szerokosc, Ustawienie) :-
  Poziom > 0,
  Poziom mod 2 =:= 1,
  rysuj_gorna_granice(Szerokosc),
  rysuj_pola_czarne(Poziom, Ustawienie),
  rysuj_pola_czarne(Poziom, Ustawienie),
  Mniejszy_Poziom is Poziom - 1,
  rysuj_poziom(Mniejszy_Poziom, Szerokosc, Ustawienie).

rysuj_poziom(0, _, _).

% Predykat rysuj_gorna_granice/1 rysujący górną granicę planszy
rysuj_gorna_granice(Dlugosc) :-
  write("+"),
  rysuj_pozioma_linie_graniczna(Dlugosc),
  write("\n").

% Predykat rysuj_pozioma_linie_graniczna/1 rysujący poziomą linię graniczną
rysuj_pozioma_linie_graniczna(Dlugosc) :-
  Dlugosc > 0,
  write("-----+"),
  Nowa_Dlugosc is Dlugosc - 1,
  rysuj_pozioma_linie_graniczna(Nowa_Dlugosc).

rysuj_pozioma_linie_graniczna(0).

% Predykat rysuj_pola_czarne/2 rysujący pola czarne
rysuj_pola_czarne(Aktualny_Indeks, Ustawienie) :-
  write("|"),
  rysuj_pole_czarne(Aktualny_Indeks, Ustawienie),
  write("\n").

% Predykat rysuj_pola_biale/2 rysujący pola białe
rysuj_pola_biale(Aktualny_Indeks, Ustawienie) :-
  write("|"),
  rysuj_pole_biale(Aktualny_Indeks, Ustawienie),
  write("\n").

% Predykat rysuj_pole_czarne/2 rysujący czarne pola z hetmanem lub puste
rysuj_pole_czarne(Aktualny_Indeks, [Glowa|Ogon]) :-
  Aktualny_Indeks =\= Glowa,
  write(":::::|"),
  rysuj_pole_biale(Aktualny_Indeks, Ogon).

rysuj_pole_czarne(Aktualny_Indeks, [Glowa|Ogon]) :-
  Aktualny_Indeks =:= Glowa,
  write(":###:|"),
  rysuj_pole_biale(Aktualny_Indeks, Ogon).

rysuj_pole_czarne(_, []).

% Predykat rysuj_pole_biale/2 rysujący białe pola z hetmanem lub puste
rysuj_pole_biale(Aktualny_Indeks, [Glowa|Ogon]) :-
  Aktualny_Indeks =\= Glowa,
  write("     |"),
  rysuj_pole_czarne(Aktualny_Indeks, Ogon).

rysuj_pole_biale(Aktualny_Indeks, [Glowa|Ogon]) :-
  Aktualny_Indeks =:= Glowa,
  write(" ### |"),
  rysuj_pole_czarne(Aktualny_Indeks, Ogon).

rysuj_pole_biale(_, []).