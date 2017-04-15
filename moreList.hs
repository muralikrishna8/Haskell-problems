
-- concat flattens a list of lists into just a list of elements.
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (\acc x -> acc++x) []
