module Lessons.Lesson02 (f,f',trd, FireExtinguisher(..), FEType(..),
    capacity, refill, length', length'') where

f :: [Integer] -> Integer
f [] = 0
f (h:_) = h

f' :: [Integer] -> Integer
f' (h1:h2:_) = h1
f' [] = 0


t1 :: (Integer, Char)
t1 = (42, 'a')

t2 :: (Integer, Int, String)
t2 = (43, -1, "labas")

trd :: (a, b, c) -> c
trd (_, _, v) = v

-- f'' :: [a] -> a
-- f'' [] = 0
-- f'' (h:_) = h


-- ADT
data FEType = A | B | C deriving Show
data FireExtinguisher = FireExtinguisher Integer FEType deriving Show

capacity :: FireExtinguisher -> Integer
capacity (FireExtinguisher c _) = c

refill :: FireExtinguisher -> FireExtinguisher
refill (FireExtinguisher _ t) = FireExtinguisher 10 t

length' :: [a] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

length'' :: [a] -> Int
length'' l = length''' l 0
    where
        length''' :: [a] -> Int -> Int
        length''' [] acc = acc
        length''' (_:t) acc = length''' t (acc + 1)