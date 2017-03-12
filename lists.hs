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

-- Find the K'th element of a list. The first element index is 1
-- elementAt [1,2,3] 2 -> 2
-- elementAt "haskell" 5 -> 'e'
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:arr) index = elementAt arr (index-1)

elementAt' arr index = arr !! (index-1)

-- Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
