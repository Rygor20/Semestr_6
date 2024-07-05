merge(IN1,IN2,OUT):-
    freeze(IN1, % Wait for IN1 to be set
    (   IN1 = [H1|R1] % If IN1 is not empty
    -> freeze(IN2, % Wait for IN2 to be set
        (   IN2 = [H2|R2] % If IN2 is not empty
        -> ( H1 =< H2 % Compare the heads of IN1 and IN2
            -> OUT = [H1|P],
               merge(R1,IN2,P)
            ;  OUT = [H2|P],
               merge(IN1,R2,P)
            )
        ;   OUT = IN1 % In case that IN2 is empty set out as IN1
        ))
    ;   OUT = IN2 % In case that IN1 is empty set out as IN1
    )).
