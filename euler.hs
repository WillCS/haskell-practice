-- Problem 1: multiples of 3 and 5
sumMultiples :: Integral n => [n] -> n -> n
sumMultiples list limit = sum [n | n <- [1..limit], any (\a -> n `mod` a == 0) list ]

solve1 :: Integral n => n
solve1 = sumMultiples [3,5] 999

-- Problem 2: Sum of even Fibonacci numbers less than four million (4,000,000)
getFibsUntil :: Integral n => ([n] -> Bool) -> [n]
getFibsUntil condition = build []
    where 
        next []  = [1]
        next [1] = [1,1]
        next fibList = sum (take 2 fibList) : fibList
        build fibList
            | not (null fibList) && condition fibList = tail fibList
            | otherwise                               = build (next fibList)

solve2 :: Integral n => n
solve2 = sum [n | n <- getFibsUntil (\n -> head n > 4000000), even n]
