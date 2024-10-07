qSort [] = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]

main = do
    print $ qSort [4, 3, 2, 1, 5]
    print $ qSort [2, 2, 3, 4, 5, 8, 1, 2, 5, 6]