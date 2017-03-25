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

-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs)++[x]

myReverse' :: [a] -> [a]
myReverse' list = myReverse'' list []
    where
        myReverse'' [] reverse = reverse
        myReverse'' (x:xs) reverse = myReverse'' xs (x:reverse)

-- Palindrome or not
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x = if ((head x) == (last x))
    then isPalindrome (tail (myReverse (tail x)))
    else False

