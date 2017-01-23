{-# language DeriveFunctor #-}
module Numeric.SD.Fix where

import qualified Data.IntMap.Strict as IM

import Data.Fix (Fix(..), cata)
-- import Data.Reify


-- | A binding environment; variables are indexed by integers
newtype Env a = Env { unEnv :: IM.IntMap a} deriving (Eq, Show)

-- | Expression ADT
-- infixl 4 :+:
-- infixl 5 :*:, :/:
-- infixr 6 :^:

data ExprF a = ConstF
             | VarF Int
             | AddF a a
             | MinusF a a
             | TimesF a a
             | DivF a a
             | ExpF a a
            -- | Neg a
            -- | ExpF a
            deriving (Eq, Functor)

-- instance Show a => Show (Expr a) where
--   show (Const x) = show x
--   show (Var i) = showVar i
--   show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
--   show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
--   show (a :/: b) = "(" ++ show a ++ " / " ++ show b ++ ")"
--   show (a :-: b) = "(" ++ show a ++ " - " ++ show b ++ ")"  
--   show (a :^: b) = show a ++ "^" ++ show b

-- | Show integer-labeled variables as consecutive letters starting from 'x'
showVar i = [v !! i] where
  v = cycle (['x' .. 'z'] ++ ['a' .. 'z'])

type Expr = Fix ExprF


add :: Expr -> Expr -> Expr
add a b = Fix (AddF a b)

eval :: Floating a => a -> Fix ExprF -> a
eval x = cata $ \e -> case e of
  ConstF -> x
  AddF a b -> a + b
  MinusF a b -> a - b
  TimesF a b -> a * b
  DivF a b -> a / b
  ExpF a b -> a**b



-- | Paramorphism

-- -- diff :: Expr -> Expr
-- diff expr = para $ case expr of 
--   -- VarF                     -> one
--   -- ZeroF                    -> zero
--   -- OneF                     -> zero
--   -- NegateF (_, x')          -> neg x'
--   AddF (_, x') (_, y')     -> add x' y'
--   TimesF (x, x') (y, y') -> add (prod x y') (prod x' y)
--   -- ExpF (x, x')             -> prod (e x) x'


-- para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para t = p where p x = t . fmap ((,) <*> p) $ unFix x
