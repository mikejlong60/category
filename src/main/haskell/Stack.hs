import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int,Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2


pop2 :: State Stack Int
pop2 = state $ \(x:xs) -> (x, xs)

push2 :: Int -> State Stack ()
push2 a = state $ \xs -> ((), a:xs)

stackManip2 :: State Stack Int
stackManip2 = do
    push2 3
    pop2
    pop2

stackStuff :: State Stack ()
stackStuff = do
    a <- pop2
    if a == 5 then push2 5
      else do
        push2 3
        push2 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip2
    if a == 100
        then stackStuff
        else return ()


stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
      then put [8,3,1]
      else put [9,2,1]

      
