import System.Environment (getArgs)

de :: Int -> Int -> (Int, Int, Int)
de a b
  | gcdVal == gcd a b = (x, y, gcdVal)
  | otherwise = error "No solution"
  where
    extendedGCD 0 b = (b, 0, 1)
    extendedGCD a b = let (z, x, y) = extendedGCD (b `mod` a) a
                      in (z, y - (b `div` a) * x, x)
    (gcdVal, x, y) = extendedGCD a b

main :: IO ()
main = do
    args <- getArgs
    let a = read (args !! 0) :: Int
        b = read (args !! 1) :: Int
        result = de a b
    putStrLn $ "Diophantine " ++ show a ++ " " ++ show b ++ " is: " ++ show result