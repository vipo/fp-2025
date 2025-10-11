-- | In this lesson we will discuss two main topics: recursion and
-- Algebraic Data Types (ADT).

module Lessons.Lesson02 (f,f',length', length'', t1, t2, trd, FireExtinguisher(..), FEType(..),
    capacity, refill) where

-- | This is the first example of a recursive function.
-- Since Haskell is ummutable, we cannot have loops like one would
-- have in C, Java or C#. Recursion is the only awailable tool.
--
-- Also it uses a technique called pattern matching: a function might
-- have a few body lines which are tested top-down. The first matching
-- the actual arguments is applied.
-- Underscore matches any element but ignores it. `:` deconstructs a list
-- into a head (single first element) and a tail (a list of elements w/o the head).
--
-- The function return the head of a list, or 0 if the list is empty.
--
-- >>> f []
-- 0
--
-- >>> f [1,2,3]
-- 1
f :: [Integer] -> Integer
f [] = 0
f (h:_) = h

-- | This pattern matching is dangerous, if only one-element list is
-- provided to the function it throws an exception.
--
-- >>> f' []
-- 0
--
-- >>> f' [1]
-- /workspaces/fp-2025/src/Lessons/Lesson02.hs:(40,1)-(41,9): Non-exhaustive patterns in function f'
--
-- >>> f' [1,2]
-- 1
f' :: [Integer] -> Integer
f' (h1:h2:_) = h1
f' [] = 0

-- | Let's implement our first recursive function! It calculates a length of
-- a list (with elements of any type `a`).     
--    
-- This function is mathematically correct but it consumes stack space to keep
-- intermediate "+" arguments. So this function will fail on "large" lists.
--
length' :: [a] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

-- | To eliminate this drawback using a [Tail call optimization](https://en.wikipedia.org/wiki/Tail_call).
-- We need the recursive call to be a final action of the function.
--
-- To achieve this we will have to introduce an additional parameter acc - accumulator where
-- we will collect intermediate results (and not on a top of the stack).
length'' :: [a] -> Int
length'' l = length''' l 0
    where
        length''' :: [a] -> Int -> Int
        length''' [] acc = acc
        length''' (_:t) acc = length''' t (acc + 1)

-- | Now let's switch to "data structures".
--
--A tuple is fixed-size structure which can contain different type elements 
t1 :: (Integer, Char)
t1 = (42, 'a')

-- | Three element's tuple
t2 :: (Integer, Int, String)
t2 = (43, -1, "labas")

-- | Tuples can be pattern-matched.
--
-- This function is also an example of "parametric polymorphism": we have no
-- concrete types, only type variables which are not know at the time of function
-- declaretion but will be know at compilation time
--
-- >>> trd (1, 10.09, "labas")
-- "labas"
trd :: (a, b, c) -> c
trd (_, _, v) = v

-- | ADT - Algerbraic data type - is way to define custom "data structures".
-- Left hand side is a type (name). Right hand side is a list of constructors.
-- Please ignore the "deriving Show" part of the declarations, it just says
-- the GHC compiler to generate a default String representation of the ADT.
-- We will be back to this question soon.
--
-- One way to define an ADT is just a "named tuple". It acts like a tuple but has a name:
-- `FireExtinguisher` is a constructor with two arguments `Integer`
-- AND `FEType`. To create a `FireExtinguisher` you have to pass both of them.
--
-- Constructors act like ordinary functions:
--
-- >>> :t FireExtinguisher 10
-- FireExtinguisher 10 :: FEType -> FireExtinguisher
data FireExtinguisher = FireExtinguisher Integer FEType deriving Show

-- | You might have a few constructors for a ADT. `FEType` can be create by calling the
-- `A` constructor OR `B` constructor OR `C` constructor.
--
-- >>> A
-- A
data FEType = A | B | C deriving Show

-- | ADTs can be pattern matched.
capacity :: FireExtinguisher -> Integer
capacity (FireExtinguisher c _) = c

-- | Usually you create new ADTs based on already existing ones.
refill :: FireExtinguisher -> FireExtinguisher
refill (FireExtinguisher _ t) = FireExtinguisher 10 t
