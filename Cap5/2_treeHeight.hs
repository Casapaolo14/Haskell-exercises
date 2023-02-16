data Tree a = Void | Node a [Tree a] 
    deriving (Eq ,Show)

myTree = Node 20 [Node 30 [Node 10 [Void]], Node 15 [Void]]

treefold :: (a -> [b] -> b) -> b -> Tree a -> b 
treefold func val Void = val
treefold func val (Node n children) = func n (map (treefold func val) children)

tHeight _ xs = 1 + maximum xs
height a = treefold tHeight (-1) a

main :: IO ()
main = do
    print $ tHeight myTree