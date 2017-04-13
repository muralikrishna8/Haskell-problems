-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))

sqrtSums' = (+) 1 $ length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]

sqrtSums'' = (+) 1 . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]

sqrtSums''' = let sqrts = scanl1 (+) $ map sqrt [1..]
                  lessThan1000 = length $ takeWhile(<1000) sqrts
              in lessThan1000 + 1
