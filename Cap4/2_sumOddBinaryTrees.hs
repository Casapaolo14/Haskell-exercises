-- ghci -XDatatypeContexts 

data BST a = Void | Node {
    val :: a,
    left, right :: BST a
    }
    deriving(Eq, Ord, Read, Show)

sumOddTree :: BST Int -> Int
sumOddTree Void = 0
sumOddTree (Node val left right)
    | odd val = val + sumOddTree left + sumOddTree right
    | otherwise = sumOddTree left + sumOddTree right

nodo1 = Node{ val = 1 , 
    left = Node { val = 2 , left = Void, right = Void} , 
    right = Node { val = 3 , left = Void, right = Void}
    }

main :: IO()
main = do
    print $ sumOddTree nodo1
