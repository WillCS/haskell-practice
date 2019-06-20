-- Problem 1: multiples of 3 and 5
sumMultiples :: Integral n => [n] -> n -> n
sumMultiples list limit = sum [n | n <- [1..limit], any (\a -> n `mod` a == 0) list ]