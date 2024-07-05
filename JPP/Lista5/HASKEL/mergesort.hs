import System.Environment (getArgs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
               in merge (mergesort ys) (mergesort zs)

main :: IO ()
main = do
    args <- getArgs
    let inputList = read (head args) :: [Int]
        sortedList = mergesort inputList
    putStrLn $ "Input List: " ++ show inputList
    putStrLn $ "Sorted List: " ++ show sortedList