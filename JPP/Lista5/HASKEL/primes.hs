import System.Environment (getArgs)

primes :: Int -> [Int]
primes n = filter isPrime [2..n]
  where
    isPrime p = null [x | x <- [2..(floor . sqrt $ fromIntegral p)], p `mod` x == 0]

main :: IO ()
main = do
    args <- getArgs
    let n = read (head args) :: Int
        primeList = primes n
    putStrLn $ "Prime numbers up to " ++ show n ++ ":"
    print primeList