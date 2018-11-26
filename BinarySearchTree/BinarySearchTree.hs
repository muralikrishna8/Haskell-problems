data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Read, Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert :: Ord a => a -> Tree a -> Tree a
insert x EmptyTree = singleton x
insert x (Node value left right)
    | x <= value = Node value (insert x left) $ right
    | otherwise = Node value left $ insert x right

findElement :: Ord a => a -> Tree a -> Bool
findElement _ EmptyTree = False
findElement x (Node value left right)
    | x == value = True
    | x < value = findElement x left
    | otherwise = findElement x right

fromList' :: Ord a => [a] -> Tree a
fromList' = foldr insert EmptyTree

fromList :: Ord a => [a] -> Tree a
fromList list = fromList' list EmptyTree
    where
        fromList' [] tree = tree
        fromList' (x:[]) tree = insert x tree
        fromList' (x:xs) EmptyTree = fromList' xs $ singleton x
        fromList' (x:xs) tree@(Node value left right)
            | x <= value =  Node value (insert x tree) right
            | otherwise = Node value left (insert x tree)
