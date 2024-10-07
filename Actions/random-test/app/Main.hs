import System.Random

randAlpha = do
    a <- randomRIO ('a', 'z')
    if a == 'z' then do
        print a
    else do
        print a
        randAlpha

main = do
    randAlpha

