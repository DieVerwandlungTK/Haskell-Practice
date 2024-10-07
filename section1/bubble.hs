bSwap [x, y]
    | x > y = [y, x]
    | otherwise = [x, y]
bSwap (x:y:xs)
    | x > y = y : bSwap (x:xs)
    | otherwise = x : bSwap (y:xs)

bSort [] = []
bSort [x] = [x]
bSort xs = bSort (take (length xs - 1 ) swapped) ++ [last swapped]
    where swapped = bSwap xs

bSwap' [x] = [x]
bSwap' (x:xs)
    | x > y = y:x:ys
    | otherwise = x:y:ys
    where (y:ys) = bSwap' xs

bSort' [] = []
bSort' xs = y : bSort' ys
    where (y:ys) = bSwap' xs

main = do
    print $ bSort [4, 3, 2, 1, 5]
    print $ bSort [2, 2, 3, 4, 5, 8, 1, 2, 5, 6]
    print $ bSort' [4, 3, 2, 1, 5]
    print $ bSort' [2, 2, 3, 4, 5, 8, 1, 2, 5, 6]