-- ghci -XDatatypeContexts 

data BST a = Void | Node {
    val :: a,
    left, right :: BST a
    }
    deriving(Eq, Ord, Read, Show)

sumTree :: BST Int -> Int
sumTree Void = 0
sumTree (Node val left right) = val + sumTree left + sumTree right

nodo1 = Node{ val = 1 , 
    left = Node { val = 2 , left = Void, right = Void} , 
    right = Node { val = 3 , left = Void, right = Void}
    }

main :: IO()
main = do
    print $ sumTree nodo1
