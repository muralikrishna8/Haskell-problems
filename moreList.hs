
-- concat flattens a list of lists into just a list of elements.
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (\acc x -> acc++x) []

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat
concatMap' :: (t -> [a]) -> [t] -> [a]
concatMap' fn = concat . map (\elem -> fn elem)

-- and takes a list of boolean values and returns True only if all the values in the list are True.
and' :: [Bool] -> Bool
and' = foldl (\x acc -> if x then acc else False) True

-- or is like and, only it returns True if any of the boolean values in a list is True.
or' :: [Bool] -> Bool
or' = foldl (\x acc -> if x then True else acc) False

any' :: (t -> Bool) -> [t] -> Bool
any' fn list = or $ map(\el -> fn el) list

all' :: (t -> Bool) -> [t] -> Bool
all' fn = and . map(\el -> fn el)
