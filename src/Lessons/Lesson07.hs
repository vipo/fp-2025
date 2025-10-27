module Lessons.Lesson07 where

-- >>> lc 
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]
lc :: [(Integer, Char)]
lc = [(a, b) | a <- [1,2,3,4], b <- ['a', 'b']]

-- >>> lc'
-- [1,1,4,4,9,9]
lc' :: [Integer]
lc' = [a * a | a <-[1,2,3], b <- [1,2] ]

-- >>> lc''
-- [1,1,2,2,3,3,4,4]
lc'' :: [Integer]
lc'' = [b | a <- [1], b <- [1,2,3,4], c <- ['a', 'z']]

-- >>> lc'''
-- []
lc''' :: [Integer]
lc''' = [a| a <- [1,2,3], b <- []]

-- >>> lc''''
-- []
lc'''' :: [Integer]
lc'''' = [a| a <- [], b <- [1,2,3]]

-- >>> lm
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]
lm :: [(Integer, Char)]
lm = do
    a <- [1,2,3,4]
    b <- ['a', 'b']
    return (a, b)

-- >>> lm'
-- [(1,'a'),(2,'a'),(3,'a'),(4,'a'),(1,'b'),(2,'b'),(3,'b'),(4,'b')]
lm' :: [(Integer, Char)]
lm' = do
    b <- ['a', 'b']
    a <- [1,2,3,4]
    return (a, b)

-- >>> mm
-- Just 84
mm :: Maybe Integer
mm = do
    a <- Just 42
    b <- Just 2
    return $ a * b

-- >>> mm'
-- Nothing
mm' :: Maybe Integer
mm' = do
    a <- Just 42
    b <- Just 2
    c <- Nothing
    return $ a * b

-- >>> mm''
-- Nothing
mm'' :: Maybe Integer
mm'' = do
    a <- Just 42
    b <- Just 2
    Just $ a * b

-- >>> em
-- Right 'a'
em :: Either String Char
em = do
    a <- Right 43
    b <- Right 'a'
    return b

-- >>> em'
-- Left "oh"
em' :: Either String Char
em' = do
    a <- Left "oh"
    b <- Right 'a'
    return b

em'' :: Either String (Integer, Char)
em'' = do
    a <- Right 43
    b <- Right 'a'
    return (a, b)

-- >>> em'''
-- Left 42
em''' :: Either Integer Integer
em''' = do
    a <- Right 43
    b <- Left 42
    return $ a * b

-- lm = do
--     a <- [1,2,3,4]
--     b <- ['a', 'b']
--     return (a, b)
lmb :: [(Integer, Char)]
lmb = [1,2,3,4] >>= (\a -> ['a', 'b'] >>= (\b -> return (a, b)))

-- mm = do
--     a <- Just 42
--     b <- Just 2
--     return $ a * b
-- >>> mmb
-- Just 84
mmb :: Maybe Integer
mmb = Just 42 >>= (\a -> Just 2 >>= (\b -> return $ a * b))

-- em = do
--     a <- Right 43
--     b <- Right 'a'
--     return b
emb :: Either String Char
emb = Right 43 >>= (\a -> Right 'a' >>= (\b -> return b))
