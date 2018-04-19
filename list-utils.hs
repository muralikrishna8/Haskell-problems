
foo :: Int -> Int
foo i = i*2

fn = map foo

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

max' x y
    | x > y = x
    | otherwise = y

compare' :: (Ord a) => a -> a -> Ordering
compare' x y
    | x > y = GT
    | x < y = LT
    | otherwise = EQ

descList x = case x of [] -> "Empty List"
                       (_:[]) -> "With one element"
                       xs -> "with more than one element"

descList' x = xs x
    where xs [] = "Empty List"
          xs [x] = "With one element"
          xs x = "with more than one element"

descList'' [] = "Empty List"
descList'' [x] = "With one element"
descList'' x = "With more than one element"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "What's wrong with you, it's an empty array"
maximum' [x] = x
maximum' (x: xs)
    | x > maximum' xs = x
    | otherwise = maximum' xs


take' :: (Ord a, Num a) => a -> [b] -> [b]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : remaining
    | otherwise = remaining
    where remaining = filter' f xs
