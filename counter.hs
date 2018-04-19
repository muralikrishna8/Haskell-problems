run :: Int -> IO ()
run c = do
  putStrLn . show $ c
  run $ c + 1
main = run 0
