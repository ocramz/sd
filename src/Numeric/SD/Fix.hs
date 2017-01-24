{-# language DeriveFunctor #-}
module Numeric.SD.Fix where

import qualified Data.IntMap.Strict as IM

import Data.Fix (Fix(..), cata)
-- import Data.Reify


-- | A binding environment; variables are indexed by integers
newtype Env a = Env { unEnv :: IM.IntMap a} deriving (Eq, Show)

-- | Expression ADT
data ExprF a = ConstF
             | VarF Int
             | a :+: a
             | a :-: a
             | a :*: a
             | a :/: a
             | a :^: a
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
showIntVar :: Int -> String
showIntVar i = [v !! i] where
  v = cycle (['x' .. 'z'] ++ ['a' .. 'z'])

type Expr = Fix ExprF


add, sub, times, frac :: Expr -> Expr -> Expr
add a b = Fix (a :+: b)
sub a b = Fix (a :-: b)
times a b = Fix (a :*: b)
frac a b = Fix (a :*: b)

var :: Int -> Expr
var = Fix . VarF

konst = Fix ConstF

eval :: Floating a => a -> Fix ExprF -> a
eval x = cata $ \e -> case e of
  ConstF -> x
  a :+: b -> a + b
  a :-: b -> a - b
  a :*: b -> a * b
  a :/: b -> a / b
  a :^: b -> a**b






kata :: (a -> b -> b) -> b -> [a] -> b
kata c z (x:xs) = c x (kata c z xs)
kata _ z []     = z

para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para c z (x:xs) = c x (xs, para c z xs)
para _ z []     = z

ana :: (a -> (b, a)) -> a -> [b]
ana u x | (a,y)       <- u x = a : ana u y

ana' :: (a -> (b, a)) -> a -> [b]
ana' u x = let (a, y) = u x
           in a : ana' u y

apo :: (a -> (b, Either [b] a)) -> a -> [b]
apo u x | (a, Right y) <- u x = a : apo u y
        | (a, Left  b) <- u x = a : b

apo' u x = case u x of (a, Right y) -> a : apo' u y
                       (a, Left b) -> a : b

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
-- para t = p where p x = t . fmap ((,) <*> p) $ unFix x
