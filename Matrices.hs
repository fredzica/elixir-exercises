type Vector = [Double]

dotProduct :: Vector -> Vector -> Double
dotProduct (v1:[]) (v2:[]) = v1 * v2
dotProduct (v1:vs1) (v2:vs2) = v1 * v2 + dotProduct vs1 vs2

-- rows are vectors
data Matrix = Matrix [Vector]
 deriving (Eq)

toList :: Matrix -> [[Double]]
toList (Matrix m) = m

matrixRows :: Matrix -> Int
matrixRows (Matrix m) = length m

matrixColumns :: Matrix -> Int
matrixColumns (Matrix m) = length (m !! 0)

zeroVector :: Int -> Vector
zeroVector s = take s $ repeat 0

zeroMatrix :: Int -> Int -> Matrix
zeroMatrix r c = Matrix $ take r $ repeat $ zeroVector c

insertAt :: a -> Int -> [a] -> [a]
insertAt nv p v = (\((s, x:xs)) -> s++[nv]++xs) $ splitAt p v

unitMatrix :: Int -> Matrix
unitMatrix i = Matrix [insertAt 1 t $ zeroVector i | t <- [0..i-1]]

getRow :: Matrix -> Int -> Vector
getRow (Matrix m) r = m !! r

get :: Int -> Int -> Matrix -> Double
get r c m = (getRow m r) !! c

set :: Int -> Int -> Matrix -> Double -> Matrix
set r c m nv = Matrix $ insertAt (insertAt nv c $ getRow m r) r $ toList m

matrixTransform :: (Int -> Int -> Double) -> Int -> Int -> Matrix
matrixTransform f r c = Matrix [[f i t | t <- [0..c-1]] | i <- [0..r-1]]

scalarMultiply :: Double -> Matrix -> Matrix
scalarMultiply v m = matrixTransform (\r c -> v * get r c m) (matrixRows m) (matrixColumns m)

matrixTranspose :: Matrix -> Matrix
matrixTranspose m = matrixTransform (\r c -> get c r m) (matrixColumns m) (matrixRows m)

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply a b = matrixTransform (\r c -> dotProduct (getRow a r) (getRow (matrixTranspose b) c)) (matrixRows a) (matrixColumns b)
