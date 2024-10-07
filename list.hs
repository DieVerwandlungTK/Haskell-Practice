length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

main = do
    print $ length' l
    print $ sum' l
    print $ product' l
    print $ take' 3 l
    print $ drop' 3 l
    print $ reverse' l
    where l = [1,2,3,4,5]