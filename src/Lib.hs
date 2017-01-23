module Lib where

import qualified Data.IntMap.Strict as IM
import Data.Maybe


-- | An binding environment; variables are indexed by integers
newtype Env a = Env { unEnv :: IM.IntMap a} deriving (Eq, Show)

-- newtype X = X { unX :: Int } deriving (Eq, Show)









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
            | (Expr a) :^: (Expr a) deriving (Eq, Show)

-- | Negate an expression
negate' :: Num a => Expr a -> Expr a
negate' (Var c)    = Const (-1) :*: Var c
negate' (Const a)  = Const (-a)
negate' (a :+: b)  = negate' a :+: negate' b
negate' (a :*: b)  = negate' a :*: b
negate' (a :^: b)  = Const (-1) :*: a :^: b
negate' (a :/: b)  = negate' a :/: b

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
simplify x          = x

fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
  where fullSimplify' cur l | cur == l = cur
                            | otherwise = let cur' = simplify cur
                                          in fullSimplify' cur' cur




-- | Differentiation
-- "Constant" and "variable" are only defined wrt a binding environment

grad :: Num a => Env e -> Expr a -> Expr a
grad _ (Const _) = Const 0
grad (Env e) (Var x) = maybe (Const 0) (\_ -> Const 1) (IM.lookup x e)
grad e (a :+: b) = grad e a :+: grad e b
grad e (a :*: b) = (a :*: grad e b) :+: (b :*: grad e a) -- product rule
grad e (a :^: Const x) = Const x :*: (a :^: (Const $ x-1)) :*: grad e a -- power rule

-- derivative (Var c)           = Const 1
-- derivative (Const x)         = Const 0
-- --product rule (ab' + a'b)
-- derivative (a :*: b)         = (a :*: (derivative b)) :+:  (b :*: (derivative a)) -- product rule
--  --power rule (xa^(x-1) * a')
-- derivative (a :^: (Const x)) = ((Const x) :*: (a :^: (Const $ x-1))) :*: (derivative a)
-- derivative (a :+: b)         = (derivative a) :+: (derivative b)
--  -- quotient rule ( (a'b - b'a) / b^2 )
-- derivative (a :/: b)         = ((derivative a :*: b) :+: (negate' (derivative b :*: a))) 
--                                :/: 
--                                (b :^: (Const 2))
