import System.Random

shuffle [] = return []
shuffle xs = do
    n <- randomRIO (0, length xs - 1) :: IO Int
    xs' <- shuffle $ take n xs ++ drop (n+1) xs
    return $ (xs !! n) : xs'

isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

bogoSort xs = do
    shuffled <- shuffle xs
    if isSorted shuffled
        then return shuffled
        else bogoSort shuffled

main = do
    xs <- shuffle [1..5]
    print xs
    print =<< bogoSort xs