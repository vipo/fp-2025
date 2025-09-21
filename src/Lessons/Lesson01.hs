-- | Module is a unit of compilation.

module Lessons.Lesson01 (i, ii, c, s, b, f, add, il, cl) where


-- >>> i + i == 84
-- True
--- >>> 5
--- 5
i :: Integer
i = 42

ii :: Int
ii = 43

c :: Char
c = 'a'

s :: String
s = "labas"

b :: Bool
b = True

f :: Integer -> Bool
f age = age >= 20

add :: Integer -> Integer -> Integer
add a b = a + b

il :: [Integer]
il = [42, 42, 42]

cl :: [Char]
cl = "labas"
