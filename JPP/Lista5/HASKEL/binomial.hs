import System.Environment (getArgs)

binomial  :: Int -> Int -> Int
binomial  n 0 = 1
binomial  n k | n == k = 1
binomial  0 _ = 0
binomial  n k = binomial  (n-1) (k-1) + binomial  (n-1) k

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        k = read (args !! 1) :: Int
        result = binomial n k
    putStrLn $ "Binomial " ++ show n ++ " " ++ show k ++ " is: " ++ show result