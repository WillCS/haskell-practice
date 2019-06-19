data Complex i = Complex { re :: i, im :: i } deriving (Show, Eq)

real :: Complex n -> n
real c@(Complex x y) = x

imaginary :: Complex n -> n
imaginary c@(Complex x y) = y

cmod :: Floating f => Complex f -> f
cmod c@(Complex x y) = sqrt (x * x + y * y)

arg :: RealFloat f => Complex f -> f
arg c@(Complex x y) = atan2 y x

instance Num n => Num (Complex n) where
    (+) (Complex a b) (Complex c d) = Complex (a + c) (b + d)
    (-) (Complex a b) (Complex c d) = Complex (a - c) (b - d)
    (*) (Complex a b) (Complex c d) = Complex (a * c - b * d) (b * c + d * a)
    signum (Complex a b) = Complex (signum a) (signum b)
    fromInteger i = Complex (fromInteger i) 0

data Matrix a = Matrix { m :: Int, n :: Int, mat :: [a] }
vector :: Int -> [a] -> Matrix a
vector len elements = Matrix { m = len, n = len, mat = elements }

(|+|) :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
Matrix m n mat1 |+| Matrix p q mat2 
    | m == p && n == q = Just Matrix { m = m, n = n, mat = (zipWith (+) mat1 mat2) }
    | otherwise        = Nothing

(|*|) :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
Matrix m n mat1 |*| Matrix p q mat2
    | n == p = Just (Matrix m q (getElemAt 1 1))
    | otherwise = Nothing
    where innerProduct a b f g = consolidateList (zipWith f a b) g
          consolidateList l@(x:xs) f
            | null xs = x
            | otherwise = f x (consolidateList xs f)
          getCol mat c = take m (drop ((c - 1) * q) mat)
          getRow mat r = reverse (getXMultiplesOfYStartingFromZ q m (r - 1) mat)
          getXMultiplesOfYStartingFromZ x y z l
            | x == 0 = (l !! z) : []
            | otherwise = (l !! (z + x * y)) : (getXMultiplesOfYStartingFromZ (x - 1) y z l)
          getElemAt y x = newElem : existingList
            where newElem = innerProduct (getRow mat1 y) (getCol mat2 x) (*) (+)
                  existingList = if newX == (negate 1) && newY == (negate 1) then [] else getElemAt newY newX
                  (newY, newX)
                    | y == m && x == q = (negate 1, negate 1)
                    | y == m = (1, x + 1)
                    | otherwise = (y + 1, x)

maybeMatrix :: Maybe (Matrix a) -> (Matrix a -> b) -> (Maybe b)
maybeMatrix (Just m) f = Just (f m)
maybeMatrix Nothing f = Nothing

printMat :: Matrix a -> [a]
printMat m@(Matrix _ _ mat) = mat