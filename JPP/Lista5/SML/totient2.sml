fun prime_factors n =
    let
        fun factors d n =
            if n = 1 then []
            else if n mod d = 0 then d :: factors d (n div d)
            else factors (d + 1) n
    in
        factors 2 n
    end;


fun pow (x, 0) = 1
  | pow (x, n) = x * pow (x, n - 1)

fun group [] = []
  | group (x::xs) = 
    let
        val (prefix, suffix) = List.partition (fn y => y = x) xs
    in
        (x::prefix) :: group suffix
    end

fun totient2 n =
    let
        fun product (p, k) = (p - 1) * pow (p, k - 1)
        val factors = prime_factors n
        val factors_grouped = group factors
        val factor_count = List.map (fn xs => (List.hd xs, List.length xs)) factors_grouped
    in
        List.foldl (fn (pk, acc) => acc * product pk) 1 factor_count
    end