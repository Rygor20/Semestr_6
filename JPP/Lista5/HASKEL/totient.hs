import System.Environment (getArgs)

totient :: Int -> Int
totient n = length [x | x <- [1..n], gcd x n == 1]

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        result = totient n
    putStrLn $ "Totient " ++ show n ++ " is: " ++ show result