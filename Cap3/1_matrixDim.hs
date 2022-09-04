numberOfLists :: [[Int]] -> Int -> [[Int]]
numberOfLists matrix ln = [x | x <- matrix, length x == ln] 

matrixDim :: [[Int]] -> (Int, Int)
matrixDim matrix = 
    if(matrix == numberOfLists matrix (length $ head matrix))
        then (length matrix, length $ head matrix)
        else (-1, -1)

main :: IO ()
main = do
    print $ matrixDim [[1,3],[5,7], [2,4]]
    print $ matrixDim [[2,3,8],[5,7,9], [2,1]]

