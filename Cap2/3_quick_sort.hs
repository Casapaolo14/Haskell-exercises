quickSort :: [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y<x] ++ [x] ++ quickSort [y | y <- xs, y>=x]

main :: IO()
main = do
    print $ quickSort [7, 1, 4, 8, 19, 3]