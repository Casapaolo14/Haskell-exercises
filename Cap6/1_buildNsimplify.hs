data QT a = C a | Q ( QT a ) ( QT a ) ( QT a ) ( QT a )
    deriving ( Eq , Show )

buildNsimplify :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
buildNsimplify q1 q2 q3 q4 
    | q1 == q2 && q1 == q2 && q1 == q3 = q1
    | otherwise = Q q1 q2 q3 q4



