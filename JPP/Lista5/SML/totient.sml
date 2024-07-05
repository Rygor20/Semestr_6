fun gcd(a, b) = 
  if b = 0 then a else gcd(b, a mod b);

fun totient n =
    let
        fun count_relative_prime m 0 count = count
          | count_relative_prime m i count =
            if gcd(m, i) = 1 then count_relative_prime m (i - 1) (count + 1)
            else count_relative_prime m (i - 1) count
    in
        count_relative_prime n n 0
    end;
