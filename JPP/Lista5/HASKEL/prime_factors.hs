import System.Environment (getArgs)

primeFactors :: Int -> [Int]
primeFactors n = factor n 2
  where
    factor n p
      | p * p > n      = [n | n > 1]
      | n `mod` p == 0 = p : factor (n `div` p) p
      | otherwise      = factor n (p + 1)

main :: IO ()
main = do
    args <- getArgs
    let n = read (head args) :: Int
        factors = primeFactors n
    putStrLn $ "Prime factors of " ++ show n ++ ":"
    print factors