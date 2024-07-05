fun binomial n 0 = IntInf.fromInt 1
  | binomial 0 k = IntInf.fromInt 0
  | binomial n k = IntInf.+ (binomial (n - 1) (k - 1), binomial (n - 1) k);
