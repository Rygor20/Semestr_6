fun sieve [] = []
  | sieve (x::xs) =
    x :: sieve (List.filter (fn y => y mod x <> 0) xs);

fun primes n =
    sieve (List.tabulate(n - 1, fn i => i + 2));
