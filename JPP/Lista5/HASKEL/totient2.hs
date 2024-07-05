import System.Environment (getArgs)

primeFactors :: Int -> [Int]
primeFactors n = factor n 2
  where
    factor n p
      | p * p > n      = [n | n > 1]
      | n `mod` p == 0 = p : factor (n `div` p) p
      | otherwise      = factor n (p + 1)

totient2 :: Int -> Int
totient2 n = product [(p - 1) * p ^ (k - 1) | (p, k) <- factorize n]
    where
        factorize n = [(p, length xs) | xs@(p:_) <- group $ primeFactors n]
          where
            group [] = []
            group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (==x) xs)

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        result = totient2 n
    putStrLn $ "Totient2 " ++ show n ++ " is: " ++ show result