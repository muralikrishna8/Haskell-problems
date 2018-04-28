import Control.Monad
main = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "What color comes to your mind when you see the number: " ++ show a
        getLine)
    sequence $ map putStrLn colors
    mapM_ putStrLn colors
