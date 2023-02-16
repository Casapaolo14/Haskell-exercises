import Data.List
import System.IO

-- programma che data una lista la scrive al contrario
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- programma che data una lista, dice se è palindroma
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs == last xs) && (isPalindrome $ init $ tail xs)

-- programma che data una lista, dice se è palindroma e restituisce la più grande sottostringa palindroma
longestPalindrome :: (Eq a) => [a] -> Maybe [a]
longestPalindrome xs
  | isPalindrome xs = Just xs
  | otherwise = case (longestPalindrome (init xs), longestPalindrome (tail xs)) of
                  (Just ys, Just zs) -> Just (if length ys > length zs then ys else zs)
                  (Just ys, Nothing) -> Just ys
                  (Nothing, Just zs) -> Just zs
                  (Nothing, Nothing) -> Nothing

-- programma in che date due liste, determina se una delle due è una sottolista dell’altra
isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist xs ys@(y:yt) = if isPrefix xs ys
                         then True
                         else isSublist xs yt

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && (isPrefix xs ys)

-- funzione in che data una lista determina la lunghezza della più lunga sottolista finale palindroma
longestPalindromicSuffix :: (Eq a) => [a] -> Int
longestPalindromicSuffix xs = maximum [length ys | ys <- suffixes xs, isPalindromic ys]

suffixes :: [a] -> [[a]]
suffixes xs = [drop n xs | n <- [0..length xs]]

isPalindromic :: (Eq a) => [a] -> Bool
isPalindromic xs = xs == reverse xs

-- funzione che data una lista determina la lunghezza della più lunga sottolista palindroma
longestPalindromicSublist :: (Eq a) => [a] -> Int
longestPalindromicSublist xs = maximum [j-i+1 | i <- [0..length xs], j <- [i..length xs-1], isPalindromic (take (j+1) (drop i xs))]

-- funzione  che  data  una  lista  restituisce  la  lista  contenente  le  sole  coppie  ordinate  
--(il  primoelemento della coppia è minore del secondo)
orderedPairs :: (Ord a) => [a] -> [(a, a)]
orderedPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys, x < y]

-- funzione che  data  una  lista  di  coppie  le  ordina,  
-- ossia scambia tra loro gli elementi di una coppia in modo che il primo sia minore del secondo

sortPairs :: (Ord a) => [(a, a)] -> [(a, a)]
sortPairs xs = [(x, y) | (x, y) <- xs, x <= y] ++ [(y, x) | (x, y) <- xs, x > y]

-- funzione che in una lista di booleani conta il numero di elementi uguali a True presenti nella lista
countTrue :: [Bool] -> Int
countTrue xs = length [x | x <- xs, x == True]

-- funzione che in una lista di interi conta il numero di elementi pari presenti nella lista
countEven :: [Int] -> Int
countEven xs = length [x | x <- xs, even x]

-- funzione che in una lista di interi conta il numero di elementi in poizione pari presenti nella lista
countEvenIndex :: [Int] -> Int
countEvenIndex [] = 0
countEvenIndex [x] = 0
countEvenIndex (x:_:xs) = 1 + countEvenIndex xs

-- funzione che in una lista di interi restituisce gli elementi in poizione pari presenti nella lista
evenIndexElements :: [Int] -> [Int]
evenIndexElements [] = []
evenIndexElements [x] = []
evenIndexElements (x:y:xs) = x : evenIndexElements xs

-- funzione che data una lista di elementi ed un valore conta il numero di volte che il valoreappare nella lista
countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences [] _ = 0
countOccurrences (x:xs) y
  | x == y = 1 + countOccurrences xs y
  | otherwise = countOccurrences xs y

{-
In Haskell, le type class Eq e Ord sono utilizzate per definire funzionalità comuni su tipi di dati che supportano l'uguaglianza e l'ordinamento.
La type class Eq fornisce una funzione == (lettura: "uguale") che può essere utilizzata per confrontare due valori di un tipo di dati. 
Un tipo di dati che appartiene alla type class Eq deve definire la funzione == per stabilire come verificare l'uguaglianza tra i suoi valori.

La type class Ord, invece, fornisce una serie di funzioni (<, <=, >, >=) per confrontare due valori di un tipo di dati e stabilire l'ordine tra di essi. 
Un tipo di dati che appartiene alla type class Ord deve definire queste funzioni per stabilire come ordinare i suoi valori.

In sintesi, la type class Eq si occupa di definire come stabilire l'uguaglianza tra valori di un tipo di dati, 
mentre la type class Ord si occupa di definire come stabilire l'ordine tra di essi
-}

numberOfLists :: [[Int]] -> Int -> [[Int]]
numberOfLists matrix ln = [x | x <- matrix, length x == ln] 

matrixDim :: [[Int]] -> (Int, Int)
matrixDim matrix = 
    if(matrix == numberOfLists matrix (length $ head matrix))
        then (length matrix, length $ head matrix)
        else (-1, -1)

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

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n-1)

combination :: (Eq a, Num a, Integral a) => a -> a -> a
combination n k 
    | k < 0 = 0
    | k > n = 0
    | otherwise = div (factorial n) (factorial(n-k) * factorial k)

main :: IO ()
main = do

    print $ sortPairs [(9,1),(6,9),(4,5)]
