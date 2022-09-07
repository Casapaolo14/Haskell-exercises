-- ghci -XDatatypeContexts 

data Tree a = Void | Node a [Tree a]
    deriving (Eq, Show)

treefold :: (a -> [b] -> b) -> b -> Tree a -> b 
treefold _ val Void = val
treefold func val (Node n children) = func n (map (treefold func val) children)

tree = Node 1 [Node 2 [Void, Void], Node 3 [Void]]


sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

sommaTree ::  a -> [b] -> b
sommaTree val lista = val + (sumList lista)

main :: IO()
main = do
    print $ treefold sommaTree 2 tree