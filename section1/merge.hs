merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort left) (mSort right)
    where h = length xs `div` 2
          left = take h xs
          right = drop h xs

main = do
    print $ mSort [4, 3, 2, 1, 5]
    print $ mSort [2, 2, 3, 4, 5, 8, 1, 2, 5, 6]