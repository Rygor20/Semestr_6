fun prime_factors n =
    let
        fun factors d n =
            if n = 1 then []
            else if n mod d = 0 then d :: factors d (n div d)
            else factors (d + 1) n
    in
        factors 2 n
    end;
