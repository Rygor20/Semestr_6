na_prawo(X, Y) :- X is Y+1.
na_lewo(X, Y) :- na_prawo(Y, X).

obok(X, Y) :- na_prawo(X, Y).
obok(X, Y) :- na_lewo(X, Y).

rybki(Kto) :-
    Domki = [
            domek(       1, Kraj1,      Kolor1,     Zwierze1,   Pije1,      Pali1),
            domek(       2, Kraj2,      Kolor2,     Zwierze2,   Pije2,      Pali2),
            domek(       3, Kraj3,      Kolor3,     Zwierze3,   Pije3,      Pali3),
            domek(       4, Kraj4,      Kolor4,     Zwierze4,   Pije4,      Pali4),
            domek(       5, Kraj5,      Kolor5,     Zwierze5,   Pije5,      Pali5)],

    /* 1 */ member(domek(1, norweg,     _,          _,          _,          _       ), Domki),

    /* 2 */ member(domek(_, brytyjczyk, czerwnoy,   _,          _,          _       ), Domki),

    /* 3 */ member(domek(A, _,          zielony,    _,          _,          _       ), Domki),
            member(domek(B, _,          biały,      _,          _,          _       ), Domki),
    na_lewo(A, B),

    /* 4 */ member(domek(_, duńczyk,    _,          _,          herbata,    _       ), Domki),

    /* 5 */ member(domek(C, _,          _,          _,          _,          light   ), Domki),
            member(domek(D, _,          _,          koty,       _,          _       ), Domki),
    obok(C, D),

    /* 6 */ member(domek(_, _,          żółty,      _,          _,          cygara  ), Domki),

    /* 7 */ member(domek(_, niemiec,    _,          _,          _,          fajka   ), Domki),

    /* 8 */ member(domek(3, _,          _,          _,          mleko,       _      ), Domki),

    /* 9 */ member(domek(I, _,          _,          _,          _,          light   ), Domki),
            member(domek(J, _,          _,          _,          woda,       _       ), Domki),
    obok(I, J),

    /* 10*/ member(domek(_, _,          _,          ptaki,      _,          filtr   ), Domki),

    /* 11*/ member(domek(_, szwed,      _,          psy,        _,          _       ), Domki),

    /* 12*/ member(domek(G, norweg,     _,          _,          _,          _       ), Domki),
            member(domek(H, _,          niebieski,  _,          _,          _       ), Domki),
    obok(G, H),

    /* 13*/ member(domek(E, _,          _,          konie,      _,          _       ), Domki),
            member(domek(F, _,          żółty,      _,          _,          _       ), Domki),
    obok(E, F),

    /* 14*/ member(domek(_, _,          _,          _,          piwo,       mentol  ), Domki),

    /* 15*/ member(domek(_, _,          zielony,    _,          kawa,       _       ), Domki),

    /* ? */ member(domek(_, Kto,        _,          rybki,      _,          _       ), Domki).