fun extended_gcd 0 b = (b, 0, 1)
  | extended_gcd a b =
    let
        val (g, x, y) = extended_gcd (b mod a) a
    in
        (g, y - (b div a) * x, x)
    end

fun de a b = 
    let
        val (g, x, y) = extended_gcd a b
    in
        (x, y, g)
    end