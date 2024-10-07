fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

fib2 n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib2 (n-1) + fib2 (n-2)

fib3 n =  case n of
    0 -> 0
    1 -> 1
    _ | n > 1 -> fib3 (n-1) + fib3 (n-2)

main = do
    print $ fib1 n
    print $ fib2 n
    print $ fib3 n
    where n = 8