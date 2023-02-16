data BST a = Void | Node {
    val :: a,
    left, right :: BST a
    }
    deriving(Eq, Ord, Read, Show)

nodo1 = Node{ val = 1 , 
    left = Node { val = 2 , left = Node { val = 7 , left = Void, right = Void}, 
        right = Node { val = 5 , left = Void, right = Void}} , 
    right = Node { val = 3 , left = Node { val = 8 , left = Void, right = Void}, 
        right = Node { val = 4 , left = Void, right = Void}}
    }

bstElem :: a -> Node a -> Boolean
bstElem k Void = False
bstElem k (Node x left right) =
    if(k == x)
        then True
    else if(k > x)
        then bstElem k right
    else bstElem k left

main :: IO ()
main = do
    print $ bstElem 8 nodo1
    print $ bstElem 9 nodo1 
