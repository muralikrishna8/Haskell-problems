import Data.List
con = concatMap (replicate 4) [1,2]

asd = sum $ takeWhile (<10000) $ map (^3) [1..]

inits' :: [a] -> [a]
inits' list = 
              let listLen = length list
              in inits'' 0 []  
              where inits'' listLen  finalList = finalList 
                    inits'' len l = l ++ (take len list) ++ (inits'' (len+1) l)

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = foldl (\acc x -> if take (length needle) x == needle then True else acc) False (tails haystack)
