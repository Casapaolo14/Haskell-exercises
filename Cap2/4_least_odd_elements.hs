quickSort :: [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y<x] ++ [x] ++ quickSort [y | y <- xs, y>=x]

leastOddElements :: [a] -> [a]
leastOddElements xs = take 2 . quickSort $ filter odd xs

main :: IO()
main = do
    print $ leastOddElements [8,1,6,7,9,3,4]