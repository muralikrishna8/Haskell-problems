phoneBook =
    [("Murali", "1234")
    ,("Sathish", "5678")
    ,("Balu", "9101")
    ,("Vishnu", "8983")
    ,("Vinay", "4315")
    ]

findElement :: Eq k => k -> [(k, v)] -> v
findElement key map = snd . head . filter (\(k, v) -> k == key) $ map

findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k, v):xs) = if k == key
                            then Just v
                            else findKey key xs
