-- filter the values in the give list based on the predicate function
-- Ex: filter' (>10) [1..12]
-- [11,12]
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:filter' f xs
    | otherwise = filter' f xs

-- filter with left fold
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred list = foldl (\acc x -> if pred x then acc ++ [x] else acc) [] list

-- filter with right fold
filter''' :: (a -> Bool) -> [a] -> [a]
filter''' pred list = foldr(\x acc -> if pred x then x:acc else acc) [] list

-- we can omit the list parameter anyways it's gonna retrun a curried function
-- filter''' pred = foldr(\x acc -> if pred x then x:acc else acc) []


