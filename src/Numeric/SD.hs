{-# language DeriveFunctor #-}
module Numeric.SD where

import Lib

import qualified Data.IntMap.Strict as IM

import Data.Maybe (fromMaybe)



-- | Expression ADT
infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a = Const a
            | Var Int
            | (Expr a) :+: (Expr a)
            | (Expr a) :-: (Expr a)
            | (Expr a) :*: (Expr a)
            | (Expr a) :/: (Expr a)
            | (Expr a) :^: (Expr a)
            | Log a (Expr a)
            | Ln (Expr a)
            | Exp (Expr a)  
            deriving (Eq, Functor)

instance Show a => Show (Expr a) where
  show (Const x) = show x
  show (Var i) = showVar i
  show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (a :/: b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (a :-: b) = "(" ++ show a ++ " - " ++ show b ++ ")"  
  show (a :^: b) = show a ++ "^" ++ show b
  show (Log b x) = "log_" ++ show b ++ "("++show x++")"
  show (Ln x) = "ln("++show x++")"  
  show (Exp x) = "exp("++show x++")"




-- | Negate an expression
negate' :: Num a => Expr a -> Expr a
negate' (Var c)    = Const (-1) :*: Var c
negate' (Const a)  = Const (-a)
negate' (a :+: b)  = negate' a :+: negate' b
negate' (a :-: b)  = b :-: a
negate' (a :*: b)  = negate' a :*: b
negate' (a :^: b)  = Const (-1) :*: a :^: b
negate' (a :/: b)  = negate' a :/: b
negate' (Log b e)  = Const (-1) :*: Log b e
negate' (Ln e)  = Const (-1) :*: Ln e
negate' x = Const (-1) :*: x


-- | Some basic simplifications
simplify :: (Eq a, Floating a) => Expr a -> Expr a
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a

simplify (Const a :*: Const b) = Const (a*b)
simplify (a :*: Const 1)         = simplify a
simplify (Const 1 :*: a)         = simplify a
simplify (_ :*: Const 0)         = Const 0
simplify (Const 0 :*: _)         = Const 0

simplify (Const a :^: Const b)       = Const (a**b)
simplify (a :^: Const 1)             = simplify a
simplify (_ :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: Const (a*b)

simplify (Const a :*: (Const b :*: expr)) = (Const $ a*b) :*: simplify expr
simplify (Const a :*: expr :*: Const b) = (Const $ a*b) :*: simplify expr
simplify (expr :*: Const a :*: Const b) = (Const $ a*b) :*: simplify expr
simplify (Const a :*: (b :+: c))        = (Const a :*: simplify b) :+: (Const a :*: simplify c)

simplify (Const 0 :/: _        ) = Const 0
simplify (Const _ :/: Const 0)   = error "Division by zero!"
simplify (Const a :/: Const b)   | a == b = Const 1 -- only when a == b
simplify (a       :/: Const 1)   = simplify a

simplify (a :/: b)  = simplify a :/: simplify b
simplify (a :^: b)  = simplify a :^: simplify b
simplify (a :*: b)  = simplify a :*: simplify b
simplify (a :+: b)  = simplify a :+: simplify b
simplify (Log b x)  = Log b (simplify x)
simplify (Ln (Exp x)) = simplify x
simplify x          = x


-- | `fullSimplify` runs `simplify` on an expression until the current input matches the last output of simplify (which ensures an expression is completely simplified). In other words, the simplified expression is the fixed point of the simplifying algoritm
fullSimplify :: (Floating a, Eq a) => Expr a -> Expr a
fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
  where fullSimplify' c l | c == l = c
                          | otherwise = let c' = simplify c
                                        in fullSimplify' c' c




-- | Differentiation
-- "Constant" and "variable" are only defined wrt a binding environment

diff :: Num a => Int -> Expr a -> Expr a
diff _ (Const _) = Const 0
diff e (Var x) | x == e = Const 1
               | otherwise = Const 0
diff e (a :+: b) = diff e a :+: diff e b
diff e (a :-: b) = diff e a :-: diff e b
diff e (a :*: b) = (a :*: diff e b) :+: (b :*: diff e a) -- product rule
diff e (a :^: Const x) = Const x :*: (a :^: (Const $ x-1)) :*: diff e a -- power rule
diff e (a :/: b) = (diff e a :*: b) :+: negate' (diff e b :*: (b :^: Const 2))
-- diff e (Log b x) = 1 :/: (x :*: Const (log b))  -- ?


-- | Gradient
grad :: (Floating a, Eq a) => Env a1 -> Expr a -> IM.IntMap (Expr a)
grad (Env e) expr = fullSimplify <$> IM.mapWithKey (\x _ -> diff x expr) e






-- | Evaluation

-- | Evaluate an Expression, given a binding Environment
eval :: Floating a => Env a -> Expr a -> a
eval _ (Const c) = c
eval e (Var x) = lookupEnv0 x e
eval e (a :+: b) = eval e a + eval e b
eval e (a :*: b) = eval e a * eval e b
eval e (a :-: b) = eval e a - eval e b
eval e (a :/: b) = eval e a / eval e b
eval e (a :^: b) = eval e a ** eval e b


evalGrad :: (Floating a1, Eq a1) => Env a1 -> Expr a1 -> IM.IntMap a1
evalGrad e expr = eval e <$> grad e expr




