{-# LANGUAGE FlexibleContexts #-}
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import Control.Monad.State

parseInput :: String -> Int
parseInput = read

insertScore :: (MonadState Int m) => Int -> m ()
insertScore input = modify (+ input)

calcScore :: (MonadState Int m) => m Int
calcScore = get

score :: (MonadState Int m, MonadIO m) => m ()
score = do
    val <- liftIO $ putStr "> " >> getLine
    let input = parseInput val
    if input == 0 then do
        score <- calcScore
        liftIO $ print ("exiting with score: " ++ show score)
    else do
        liftIO $ print ("Given Input is: " ++ show input)
        insertScore input
        score

main = do
    -- this Buffering is for https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=846279
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    evalStateT score 0
