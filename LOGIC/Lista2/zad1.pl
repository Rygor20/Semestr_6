% 1. member(X, L), który jest prawdziwy, gdy X jest elementem listy L
% 2. append(L1, L2, L3), który jest prawdziwy, gdy lista L3 jest połączeniem (konkatenacją) list L1 i L2
% 3. select(X, L1, L2), który jest prawdziwy, gdy lista L2 powstaje z listy L1 przez wyjęcie jednego elementu X

% append([], L, L).
% append([X | L1], L2, [X | L3]) :-
%     append(L1, L2, L3).



środkowy(L, X) :-
    append(Left, [X|Right], L), % Podziel listę na część lewą (Left), środkowy element (X) i część prawą (Right)
    length(Left, N), % Oblicz długość części lewej
    length(Right, N). % Oblicz długość części prawej, powinna być taka sama jak lewej