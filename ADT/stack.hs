--
-- stack.hs
--

module ADT.Stack where

data Stack a = Empty | Top a (Stack a)

empty :: Stack a -> Bool
empty Empty = True
empty (Top _ _) = False

top :: Stack a -> Maybe a
top Empty = Nothing
top (Top x _) = Just x

pred :: Stack a -> Stack a
pred Empty = Empty
pred (Top _ stack) = stack

push :: Stack a -> a -> Stack a
push stack x = Top x stack

pop :: Stack a -> Stack a
pop (Top _ stack) = stack

list :: Stack a -> [a]
list Empty = []
list (Top x stack) = x:(list stack)

serlz :: (Show a) => Stack a -> String
serlz Empty = ""
serlz (Top x stack) = (show x) ++
    if (empty stack) then "" else ":" ++ (serlz stack)

instance (Show a) => Show (Stack a) where
    -- show stack = show (list stack)
    show stack = "(" ++ (serlz stack) ++ ")"

stack_test = do
    let s0 = Empty :: Stack Integer
    let s1 = push s0 1
    let s2 = push s1 2
    let s21 = push s1 21
    let s3 = push s2 3
    let s31 = push s2 31
    let s4 = push s3 4
    putStrLn (show s1)
    putStrLn (show s2)
    putStrLn (show s21)
    putStrLn (show s3)
    putStrLn (show s31)
    putStrLn (show s4)
