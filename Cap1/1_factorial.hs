factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n-1)


main :: IO()
main = do
    print $ factorial 10000