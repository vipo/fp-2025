-- | Notes taken by Jonas Grybė
--
-- VARIABLE BINDING PROBLEM:
-- When substituting in lambda calculus, we must change only FREE variables, not BOUND ones.
-- Example: (λx.λy.x(xy)) - x and y are bound, we can't just replace them.
--
-- TWO SOLUTIONS:
-- 1. Barendregt Convention: All variables must be unique (eliminates name conflicts).
-- 2. De Bruijn Indices: Don't use names - use numbers indicating distance to binding lambda.
--    Examples: λx.x → λ.0  |  λx.λy.x(xy) → λ.λ.1(10)  |  λx.λy.b(yx) → λ.λ.2(01)
--
-- EVALUATION STRATEGIES:
-- - Full Beta Reduction: Reduce anywhere (messy, can use function or argument first)
-- - Call by Value: Evaluate arguments before passing (Java, C#, C)
-- - Call by Name: No reduction inside abstractions (Haskell)

{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson14 () where
import qualified Data.List as L


-- | Named lambda calculus representation. Grammar: term = x | λx.t | tt
data Term = Var String | Abs String Term | App Term Term

instance Show Term where
  show :: Term -> String
  show (Var n) = n
  show (Abs n t) = concat ["(/|", n, ".", show t, ")"]
  show (App t1 t2) = concat ["(", show t1, " ", show t2, ")"]

-- | Church boolean TRUE: λt.λf.t (returns first argument)
-- >>> show tru
-- "(/|t.(/|f.t))"
tru :: Term
tru = Abs "t" (Abs "f" (Var "t"))

-- | Church boolean FALSE: λt.λf.f (returns second argument)
-- >>> show fls
-- "(/|t.(/|f.f))"
fls :: Term
fls = Abs "t" (Abs "f" (Var "f"))

-- | Church numeral ZERO: λs.λz.z (successor applied 0 times)
-- Based on Peano numbers: z=zero, s=successor function.
-- >>> show c0
-- "(/|s.(/|z.z))"
c0 :: Term
c0 = Abs "s" (Abs "z" (Var "z"))

-- | Church numeral TWO: λs.λz.s(s z) (successor applied 2 times)
-- >>> show c2
-- "(/|s.(/|z.(s (s z))))"
c2 :: Term
c2 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))

-- | Builds left-associative applications from a list of terms.
-- Allows us to write expression over expression, building complex applications.
-- Takes multiple terms and combines them: apps [t1, t2, t3] = ((t1 t2) t3)
apps :: [Term] -> Term
apps [] = error "Empty application"
apps [_] = error "Two terms needed for application"
apps (t1 : t2 : ts) = apps' (App t1 t2) ts
  where
    apps' t [] = t
    apps' t (x : xs) = apps' (App t x) xs

-- | Logical AND for Church booleans: λb.λc.b c false
-- >>> show land
-- "(/|b.(/|c.((b c) (/|t.(/|f.f)))))"
land :: Term
land = Abs "b" $ Abs "c" $ apps [Var "b", Var "c", fls]

-- | Example expression: (land true true)
-- >>> show expr
-- "(((/|b.(/|c.((b c) (/|t.(/|f.f))))) (/|t.(/|f.t))) (/|t.(/|f.t)))"
expr :: Term
expr = apps [land, tru, tru]

-- plus :: Term
-- plus = Abs "m" $ Abs "n" $ Abs "s" $ Abs "z" $ apps [Var "m", Var "s", apps [Var "n", Var "s", Var "z"]]

-- | Nameless lambda terms using De Bruijn indices (no variable names, just numbers).
data ITerm
  = IVar Int
  | IAbs ITerm
  | IApp ITerm ITerm
  deriving (Eq)

instance Show ITerm where
  show :: ITerm -> String
  show (IVar i) = show i
  show (IAbs t) = concat ["(/|.", show t, ")"]
  show (IApp t1 t2) = concat ["(", show t1, " ", show t2, ")"]

-- | Converts named terms to De Bruijn indices.
-- ctx: free variable names (context) - not bound within the term.
-- Walk the term with empty stack initially. When numbering, we go from inside to inside,
-- stacking variable names as we enter abstractions (n : stack).
-- findInd checks stack first (bound vars), then context (free vars).
-- For context variables: take context position + length of stack to renumber properly.
-- >>> deBruijnIndices [] tru
-- (/|.(/|.1))
-- >>> deBruijnIndices [] fls
-- (/|.(/|.0))
-- >>> deBruijnIndices ["a", "b", "c", "d"] $ Abs "s" (Abs "z" (Var "d"))
-- (/|.(/|.5))
deBruijnIndices :: [String] -> Term -> ITerm
deBruijnIndices ctx t = walk [] t
  where
    walk stack (Var n) = IVar (findInd stack n)
    walk stack (Abs n t) = IAbs (walk (n : stack) t)
    walk stack (App t1 t2) = IApp (walk stack t1) (walk stack t2)
    findInd stack n =
      case (n `L.elemIndex` stack, n `L.elemIndex` ctx) of
        (Just i, _) -> i
        (Nothing, Just i) -> L.length stack + i
        _ -> error $ "No index for free variable " ++ n

-- | Shifts free variables by d. Cutoff c tracks binding depth.
-- Variables with index >= c are free and get shifted.
termShift :: Int -> ITerm -> ITerm
termShift d = walk 0
  where
    walk c (IVar x)
      | x >= c = IVar (x + d)
      | otherwise = IVar x
    walk c (IAbs t') = IAbs (walk (c + 1) t')
    walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

-- | Substitutes term s for variable j. Core operation for beta reduction.
-- When x == j+c, replace with s (shifted by c for binders).
termSubst :: Int -> ITerm -> ITerm -> ITerm
termSubst j s = walk 0
  where
    walk c (IVar x)
      | x == j+c = termShift c s
      | otherwise = IVar x
    walk c (IAbs t') = IAbs (walk (c + 1) t')
    walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

-- | Top-level substitution for beta reduction: (λ.t) s → [s/0]t
-- Shift s up, substitute, shift result down.
termSubstTop :: ITerm -> ITerm -> ITerm
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- | Only abstractions are values in call-by-name.
isVal :: ITerm -> Bool
isVal (IAbs _) = True
isVal _ = False

-- | Single-step evaluator (call-by-name, like Haskell).
-- From Pierce's "Types and Programming Languages".
-- Each eval call returns a shorter expression.
-- >>> eval $ deBruijnIndices [] expr
-- ((/|.(((/|.(/|.1)) 0) (/|.(/|.0)))) (/|.(/|.1)))
-- >>> eval $ eval $ deBruijnIndices [] expr
-- (((/|.(/|.1)) (/|.(/|.1))) (/|.(/|.0)))
-- >>> eval $ eval $ eval $ deBruijnIndices [] expr
-- ((/|.(/|.(/|.1))) (/|.(/|.0)))
-- >>> eval $ eval $ eval $ eval $ deBruijnIndices [] expr
-- (/|.(/|.1))
eval :: ITerm -> ITerm
eval (IApp (IAbs t') v2) | isVal v2 = termSubstTop v2 t'
eval (IApp v1 t2) | isVal v1 = IApp v1 (eval t2)
eval (IApp t1 t2) = IApp (eval t1) t2
eval t = error $ "No rule to apply for: " ++ show t