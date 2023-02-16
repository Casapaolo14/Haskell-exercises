data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)

data Mat a = Mat {
    nexp :: Int,
    mat :: QT a
} deriving (Eq, Show)

QTMatrix = Mat 3 (Q (C 5) (C 6) (Q (C 7) (C 8) (C 2) (C 1)) (C 4))
QTMatrix2 = Mat 1 (Q (C 3) (C 6) (C 6) (C 2))

transposed (C x) = (C x)
transposed (Q qt1 qt2 qt3 qt4) = Q (transposed qt1) (transposed qt3) (transposed qt2) (transposed qt4)

transpose (Mat nexp mat) = Mat nexp (transposed mat)


simmetric matrix = 
    if(matrix == transpose matrix)
        then True
    else False

main :: IO ()
main = do
    print $ simmetric QTMatrix1
    print $ simmetric QTMatrix2