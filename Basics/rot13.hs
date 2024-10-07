import Data.Char

rot13 [] = []
rot13 (x:xs) = x' : rot13 xs
    where
        x' = chr $ (ord x + 13 - ord 'a') `mod` 26 + ord 'a'

main = do
    print $ rot13 "hello"
    print $ rot13 "uryyb"
