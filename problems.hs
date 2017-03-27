fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib(n-1) + fib(n-2)

fact :: Int -> Int
fact n = case n of
         0 -> 1
         _ -> n * fact(n-1)

fact' :: Int -> Int
fact' n = if(n==0) then 1
          else n * fact'(n-1)


size :: [Int] -> Int
size [] = 0
size (x:xs) = 1 + size(xs)

