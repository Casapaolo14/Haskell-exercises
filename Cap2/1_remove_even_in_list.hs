-- Even Index of List
evenPosList :: [a] -> [a]
evenPosList [] = []
evenPosList (x : xs) = 
    if(length xs > 1)
        then (take 1 xs) ++ evenPosList(drop 1 xs)
        else xs

-- Odd Index of List
oddPosList :: [a] -> [a]
oddPosList [] = []
oddPosList (x : xs) = 
    if(length xs > 0)
        then x : oddPosList(drop 1 xs)
        else x : []


main :: IO ()
main = do
    print $ evenPosList [1,3,5,7,9,11,13,15,17]
    print $ oddPosList [1,3,5,7,9,11,13,15,17,19]