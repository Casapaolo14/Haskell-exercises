data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)

quadtree1 = Q (C 5) (C 2) (C 8) (C 2)
quadtree2 = C 4
quadtree3 = Q (C 7) (Q (C 3) (C 6) (C 9) (C 3)) (C 4) (C 2)
quadtree4 = Q (C 9) (Q (C 2) (C 1) (C 5) (C 3)) (Q (C 6) (C 8) (C 4) (C 11)) (C 7)

mapquadtree f (C x) = C (f x)
mapquadtree f (Q qt1 qt2 qt3 qt4) = Q (mapquadtree qt1) (mapquadtree qt2) (mapquadtree qt3) (mapquadtree qt4)