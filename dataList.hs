-- intersperse takes an element and a list and then puts that element in between each pair of elements in the list
-- intersperse '.' "MONKEY" -> "M.O.N.K.E.Y"
-- intersperse 0 [1..6] -> [1,0,2,0,3,0,4,0,5,0,6]
intersperse :: a -> [a] -> [a]
intersperse elem = init . concat . map (\el -> el : [elem])

intersperse' :: a -> [a] -> [a]
intersperse' elem = init . foldr (\x acc -> x:elem:acc) []

