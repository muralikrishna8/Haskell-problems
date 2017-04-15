
-- concat flattens a list of lists into just a list of elements.
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (\acc x -> acc++x) []

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat
concatMap' :: (t -> [a]) -> [t] -> [a]
concatMap' fn = concat . map (\elem -> fn elem)
