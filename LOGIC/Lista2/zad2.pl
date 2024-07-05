% member(X, [X | _]).
% member(X, [_ | L]) :-
%     member(X, L).

% append([], L, L).
% append([X | L1], L2, [X | L3]) :-
%     append(L1, L2, L3).

% select(X, [X | L], L).
% select(X, [Y | L1], [Y | L2]) :-
%     select(X, L1, L2).



jednokrotnie(X, L) :-
    select(X, L, Rest), % Wybierz X z listy L
    \+ member(X, Rest). % Upewnij się, że X nie występuje już w pozostałej części listy

dwukrotnie(X, L) :-
    append(_, [X|Right], L), % Wybierz X z listy L
    jednokrotnie(X, Right). % Sprawdź, czy X występuje teraz jednokrotnie w L
