-- ghci -XDatatypeContexts 

data Tree a = Void | Node a [Tree a] 
    deriving (Eq, Show)

myTree = Node 1 [Node 2 [Void, Void], Node 3 [Void]]

treefold :: (a -> [b] -> b) -> b -> Tree a -> b 
treefold func val Void = val
treefold func val (Node n children) = func n (map (treefold func val) children)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

sommaTree ::  a -> [b] -> b
sommaTree val lista = val + (sumList lista)

main :: IO()
main = do
    print $ treefold sommaTree 2 tree