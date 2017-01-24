import Control.Monad


type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')


in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second


in4 :: KnightPos -> [KnightPos]
in4 start = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    moveKnight third


in5 :: KnightPos -> [KnightPos]
in5 start = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    fourth <- moveKnight third
    moveKnight fourth


in6 :: KnightPos -> [KnightPos]
in6 start = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    fourth <- moveKnight third
    fifth <- moveKnight fourth
    moveKnight fifth


canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
