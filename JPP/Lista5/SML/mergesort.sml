fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x::xs, y::ys) = if x <= y then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs = 
    let
        val n = List.length xs div 2
        val ys = List.take(xs, n)
        val zs = List.drop(xs, n)
    in
        merge (mergesort ys, mergesort zs)
    end