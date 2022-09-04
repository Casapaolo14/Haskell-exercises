-- Odd Index of List
oddPosList :: [Int] -> [Int]
oddPosList [] = []
oddPosList (x : xs) = 
    if(length xs > 0)
        then x : oddPosList(drop 1 xs)
        else x : []

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumOddElements :: [Int] -> Int
sumOddElements list = sumList(oddPosList(list))

main :: IO ()
main = do
    print $ sumOddElements [1,3,5,7,2,4,8,9]