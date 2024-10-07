data Point = Point { px :: Int, py :: Int } deriving Show
data Rect = Rect { rx :: Int, ry :: Int, rw :: Int, rh :: Int } deriving Show

contains r p =
    px p >= rx r && px p < rx r + rw r &&
    py p >= ry r && py p < ry r + rh r

main = do
    print $ contains (Rect 2 2 3 3) (Point 1 1)
    print $ contains (Rect 2 2 3 3) (Point 2 2)
    print $ contains (Rect 2 2 3 3) (Point 3 3)
    print $ contains (Rect 2 2 3 3) (Point 4 4)
    print $ contains (Rect 2 2 3 3) (Point 5 5)