main = print $ filter (\x -> 600851475143  `mod` x == 0) $ filter isPrime [2 .. isqrt 600851475143]

isPrime k = null [ x | x <- [2..isqrt k], k `mod`x  == 0]

isqrt = floor. sqrt . fromIntegral