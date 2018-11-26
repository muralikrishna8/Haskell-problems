removeMultiple :: Int -> [Int] -> [Int]
removeMultiple n = filter (\x -> x `mod` n /= 0)

getPrimes :: [Int]
getPrimes = getPrimes' [2..]
    where
        getPrimes' (x:xs) = x : getPrimes' (removeMultiple x xs)


getPrimesOpt = getPrimesOpt' [2..]
    where getPrimesOpt' (x:xs) = x : getPrimesOpt' [y | y <- xs, y `mod` x /= 0]
