columns :: [[Int]] -> [Int]
columns (x:[]) = x
columns (x:xs) = zipWith (+) x (columns xs) 

main :: IO ()
main = do
    print $ columns [[1,3], [5,7], [3,4]]
    print $ columns [[2,3,8], [5,7,9], [2,3,1]]
    --print prova