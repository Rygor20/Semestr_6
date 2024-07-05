:- consult(zad1).

split(IN, OUT1, OUT2) :-
    freeze(IN, % Wait for IN to be set
    (   IN = [H1, H2 | R] % If IN has at least two elements
        ->  (
            OUT1 = [H1 | R1], % Add H1 to OUT1
            OUT2 = [H2 | R2], % Add H2 to OUT2
            split(R, R1, R2) % Recursively split the rest of the list
        )
        ;   (IN = [H1] % If IN has exactly one element
        ->  (
            OUT1 = [H1], % Add H1 to OUT1
            OUT2 = [] % Set OUT2 to empty
        )
        ;   ( % If IN is empty
            OUT1 = [], % Set OUT1 to empty
            OUT2 = [] % Set OUT2 to empty
        ))
    )).

merge_sort(IN, OUT) :-
    freeze(IN, % Wait for IN to be set
    (   IN = [_ | R] % If IN is non-empty
        ->  freeze(R, % Wait for R (the rest of the list) to be set
            (   R = [_ | _] % If R is non-empty (IN has at least two elements)
                ->  (
                    split(IN, L1, L2), % Split IN into L1 and L2
                    merge_sort(L1, S1), % Recursively sort L1
                    merge_sort(L2, S2), % Recursively sort L2
                    merge(S1, S2, OUT) % Merge the sorted lists S1 and S2 into OUT
                )
                ;   IN = OUT % If IN has only one element, set OUT as IN
            )
        ;   OUT = [] % If IN is empty, set OUT as empty
        )
    )).