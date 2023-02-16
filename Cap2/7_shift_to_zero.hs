shiftToZero :: [a] -> [a]
shiftToZero xs = map (subtract (minimum xs)) xs

main :: IO ()
main = do
    print $ shiftToZero [7,5,2,4]