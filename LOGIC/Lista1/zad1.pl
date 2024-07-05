mezczyzna(jan).
mezczyzna(michal).
mezczyzna(krzysztof).
mezczyzna(artur).
mezczyzna(adam).
mezczyzna(dariusz).

kobieta(anna).
kobieta(marta).
kobieta(ewa).
kobieta(izabela).
kobieta(weronika).

ojciec(michal,jan).
ojciec(krzysztof,adam).
ojciec(krzysztof,ewa).
ojciec(krzysztof,izabela).
ojciec(dariusz,michal).
ojciec(dariusz,artur).
ojciec(dariusz,adam).

matka(anna,jan).
matka(ewa, dariusz).
matka(marta,adam).
matka(marta,ewa).
matka(marta,izabela).
matka(weronika,michal).
matka(weronika,artur).
matka(weronika,adam).

rodzic(X, Y) :- matka(X, Y) ; ojciec(X, Y). /* X jest rodzicem Y */
/* diff(X, Y) :- X \= Y. */

jest_matka(X) :- matka(X,_).    /* X jest matką */
jest_ojcem(X) :- ojciec(X,_).   /* X jest ojcem */
jest_synem(X) :-                /* X jest synem */
    mezczyzna(X), 
    rodzic(_,X).
siostra(X,Y) :-                 /* X jest siostrą Y */
    kobieta(X), 
    rodzic(Z,X), 
    rodzic(Z,Y), 
    X \= Y.
    /* diff(X,Y). */
dziadek(X,Y) :-                 /* X jest dziadkiem Y */
    mezczyzna(X), 
    rodzic(X,Z), 
    rodzic(Z,Y).
rodzenstwo(X,Y) :-              /* X i Y są rodzeństwem */
    rodzic(Z,X), 
    rodzic(Z,Y), 
    X \= Y.
    /* diff(X,Y). */