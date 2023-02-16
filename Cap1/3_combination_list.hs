factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n-1)

combination :: (Eq a, Num a, Integral a) => a -> a -> a
combination n k 
    | k < 0 = 0
    | k > n = 0
    | otherwise = div (factorial n) (factorial(n-k) * factorial k)

combination_list :: Integral a => a ->[a]
combination_list n = map (combination n) [1..n]

main :: IO()
main = do
    print $ combination_list 4

