-- import Distribution.Compat.Lens (_1)

data QT a = C a | Q ( QT a ) ( QT a ) ( QT a ) ( QT a )
    deriving ( Eq , Show )

buildNsimplify :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
buildNsimplify q1 q2 q3 q4 
    | q1 == q2 && q1 == q2 && q1 == q3 = q1
    | otherwise = Q q1 q2 q3 q4

simplify :: Eq a => QT a -> QT a
simplify qt = case qt of
    C _ -> qt
    Q q1 q2 q3 q4 -> buildNsimplify (simplify q1) (simplify q2) (simplify q3) (simplify q4)



