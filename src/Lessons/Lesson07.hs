module Lessons.Lesson07 where
import qualified Control.Applicative as value

-- >>> lc 
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]

-- | This function takes one list of integers and one list of characters.
-- | It iterates through both lists and produces every possible combination of pairs.
-- | In a list comprehension, the `|` symbol means **“such that”**.
-- | It separates the output expression (before the `|`) from the generators and filters (after the `|`).
-- |
-- | You can read this comprehension as:
-- | “All pairs (a,b) such that a comes from [1,2,3,4] and b comes from ['a','b']”.
-- | The comprehension automatically combines all possible values, creating a cross-product.
lc :: [(Integer, Char)]
lc = [(a, b) | a <- [1,2,3,4], b <- ['a', 'b']]

-- >>> lc'
-- [1,1,4,4,9,9]

-- | This comprehension takes each element `a`, squares it, and repeats the process for every `b` in the second list.
-- | Notice that there are 3 values of `a` and 2 values of `b`, giving 3 × 2 = 6 results in total.
lc' :: [Integer]
lc' = [a * a | a <-[1,2,3], b <- [1,2] ]

-- >>> lc''
-- [1,1,2,2,3,3,4,4]


-- | Here, even though `c` is unused in the output, it still affects the repetition count.
-- | List comprehensions expand their generators from left to right.
-- | Each generator multiplies the total number of results.
--
-- | The total number of combinations is 1 × 4 × 2 = 8.
-- | Although `c` doesn’t appear in the output, Haskell still iterates over it for every (a,b) pair.
-- | Each `b` is therefore repeated twice — once for 'a' and once for 'z'.
-- | In general, every new generator multiplies the total number of results, even if you never use that variable later.
lc'' :: [Integer]
lc'' = [b | a <- [1], b <- [1,2,3,4], c <- ['a', 'z']]

-- >>> lc'''
-- []

-- | If any list in a comprehension is empty, the entire expression collapses to an empty list.
-- | Once a generator like `b <- []` appears, the comprehension produces no output at all.
lc''' :: [Integer]
lc''' = [a| a <- [1,2,3], b <- []]

-- | Lists, like IO and STM, form monoids — they have an identity (the empty list) and a way to combine results.
-- | The empty list `[]` represents “no result”.
-- | If any generator in a comprehension is empty, the overall result is also an empty list.

lc'''' :: [Integer]
lc'''' = [a| a <- [], b <- [1,2,3]]

-- >>> lm
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b')]

-- | This example does exactly the same as `lc`, but uses `do`-notation.
-- | Each `<-` extracts a value from a list.
-- | The `return` statement wraps a value back into a list at the end.
lm :: [(Integer, Char)]
lm = do
    a <- [1,2,3,4]
    b <- ['a', 'b']
    return (a, b)

-- >>> lm'
-- [(1,'a'),(2,'a'),(3,'a'),(4,'a'),(1,'b'),(2,'b'),(3,'b'),(4,'b')]
-- | Order of extraction matters.
-- | Changing the order of `<-` changes the nesting of loops.
lm' :: [(Integer, Char)]
lm' = do
    b <- ['a', 'b']
    a <- [1,2,3,4]
    return (a, b)

-- | IO, STM, List, and Maybe are all monads — each represents a context or a container for computations.
-- | For the Maybe monad, `Just` represents a successful value, and `Nothing` means failure or absence of value.
-- | `return` wraps a plain value back into the monadic context (Just/Nothing).
-- | If any step in the chain produces `Nothing`, the entire computation short-circuits and returns `Nothing`.
-- >>> mm
-- Just 84
mm :: Maybe Integer
mm = do
    a <- Just 42
    b <- Just 2
    return $ a * b

-- | Here, the line `c <- Nothing` causes the entire computation to stop.
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
-- | In this example, you could also write `Just $ a * b` instead of `return $ a * b`,
-- | because for the Maybe monad, `return = Just`.
-- | Both expressions wrap a pure result into the Maybe context.
mm'' :: Maybe Integer
mm'' = do
    a <- Just 42
    b <- Just 2
    Just $ a * b

-- | The Either monad behaves similarly to Maybe but carries an error message on the Left side.
-- | The Left value represents a failure, and the Right value represents success.
-- | When a Left value appears, the computation stops immediately.
-- >>> em
-- Right 'a'
em :: Either String Char
em = do
    a <- Right 43
    b <- Right 'a'
    return b

-- | Here, the first Left encountered (“oh”) stops the entire computation.
-- | The remaining steps are skipped, and the Left value is returned as the result.
-- >>> em'
-- Left "oh"
-- | The first `Left` encountered ends the chain.
em' :: Either String Char
em' = do
    a <- Left "oh"
    b <- Right 'a'
    return b

-- | This example combines two successful Right values into a tuple.
-- | Since both succeed, the result is wrapped as a Right (43,'a')
em'' :: Either String (Integer, Char)
em'' = do
    a <- Right 43
    b <- Right 'a'
    return (a, b)

-- | This example demonstrates short-circuiting behavior again.
-- | When a Left value appears, evaluation stops immediately.
-- | This happens because during compilation, `do`-notation is desugared into chained binds (>>=),
-- | and the definition of >>= for Either immediately returns the first Left it encounters.
-- >>> em'''
-- Left 42
em''' :: Either Integer Integer
em''' = do
    a <- Right 43
    b <- Left 42
    return $ a * b


-- | Here we rewrite `lm` without `do`-notation, using the bind operator (>>=) explicitly.
-- | Each `>>=` extracts a value from a list and passes it into a lambda function.
-- | Inside the lambda, `return` wraps the result back into a list.
-- | The overall effect is identical to nested loops.
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

-- | Here we rewrite `lm` without `do`-notation, using the bind operator (>>=) explicitly.
-- | Each `>>=` extracts a value from a list and passes it into a lambda function.
-- | Inside the lambda, `return` wraps the result back into a list.
-- | The overall effect is identical to nested loops.
mmb :: Maybe Integer
mmb = Just 42 >>= (\a -> Just 2 >>= (\b -> return $ a * b))

-- | Monad should follow three laws: left identity, right identity and associativity.
-- | This example shows how the Either monad looks without using `do`-notation.
-- |
-- | Step-by-step:
-- | 1. Start with `Right 43`. This is a successful value, so it’s passed into the first lambda as `a`.
-- | 2. Inside the lambda, `Right 'a'` is another successful value, which is passed to the next lambda as `b`.
-- | 3. Finally, `return b` wraps `'a'` back into the monad, producing `Right 'a'`.
-- | 4. If at any point a Left value appeared, the chain would stop immediately, returning that Left.
-- em = do
--     a <- Right 43
--     b <- Right 'a'
--     return b
emb :: Either String Char
emb = Right 43 >>= (\a -> Right 'a' >>= (\b -> return b))
