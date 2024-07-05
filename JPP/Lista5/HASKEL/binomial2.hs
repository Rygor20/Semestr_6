import System.Environment (getArgs)

pascal :: [[Integer]]
pascal = [1] : map nextRow pascal
  where nextRow row = zipWith (+) (row ++ [0]) ([0] ++ row)

binomial2 :: Int -> Int -> Integer
binomial2 n k = (pascal !! n) !! k

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        k = read (args !! 1) :: Int
        result = binomial2 n k
    putStrLn $ "Binomial2 " ++ show n ++ " " ++ show k ++ " is: " ++ show result