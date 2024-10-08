fact 0 = return 0
fact 1 = return 1
fact n = do
    n' <- fact (n-1)
    return (n*n')

main = do
    print =<< fact 5
    print =<< fact 10