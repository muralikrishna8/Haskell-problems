-- Duplicate the elements of a list
-- Example dupli [1,2,3] -> [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' list = concat [[x, x] | x <- list]

-- Repeat a number for given number of times
repeat' 0 _ = []
repeat' rep val = val:repeat' (rep-1) val


-- Replicate the elements of a list a given number of times
-- repli :: [a] -> Int -> [a]
repli [] _ = []
repli list rep = concat [replicate rep x | x <- list]

repli' [] _ = []
repli' list rep = concat [repeat' rep x | x <- list]
    where
        repeat'' = repeat' 

