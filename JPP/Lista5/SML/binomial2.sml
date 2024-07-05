fun binomial2 n k =
    let
        fun next_row row =
            let
                val row' = 0 :: row
                val row'' = row @ [0]
            in
                ListPair.map op+ (row', row'')
            end;

        fun generate_pascal n =
            let
                fun generate 0 rows = rev rows
                  | generate n rows = generate (n - 1) (next_row (hd rows) :: rows)
            in
                generate n [[IntInf.fromInt 1]]
            end;

        val row_n = List.nth (generate_pascal n, n);

        val result = List.nth (row_n, k);

    in
        result
    end;