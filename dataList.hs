-- intersperse takes an element and a list and then puts that element in between each pair of elements in the list
-- intersperse '.' "MONKEY" -> "M.O.N.K.E.Y"
-- intersperse 0 [1..6] -> [1,0,2,0,3,0,4,0,5,0,6]
intersperse :: a -> [a] -> [a]
intersperse elem = init . concat . map (\el -> el : [elem])

intersperse' :: a -> [a] -> [a]
intersperse' elem = init . foldr (\x acc -> x:elem:acc) []


-- intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
-- intercalate " " ["hey","there","guys"] -> "hey there guys"  
-- intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] --> [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]  
intercalate :: [a] -> [[a]] -> [a]
intercalate list lists = (foldr (\elem acc -> elem++list++acc) [] $ init lists) ++ last lists
