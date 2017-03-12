-- Find the first element of a list
-- myLast [1,2,3,4] -> 4
-- myLast ['x', 'y', 'z'] -> 'z'

myLast :: [a] -> a
myLast [a] = a
myLast (_:xs) = myLast xs

-- Find the last but on element of a list
-- myButLast [1,2,3,4] -> 3
-- myButLast ['x', 'y', 'z'] -> 'y'

myButLast :: [a] -> a
myButLast (x:[_]) = x
myButLast (_:xs) = myButLast xs
