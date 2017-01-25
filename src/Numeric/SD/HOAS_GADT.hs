{-# language GADTs, TypeFamilies #-}
module Numeric.SD.HOAS_GADT where
-- | From : Combining Deep and Shallow Embedding for EDSL - Josef Svenningsson and Emil Axelsson - http://www.cse.chalmers.se/~josefs/publications/TFP12.pdf

import Lib



data Expr a where
  Const :: a -> Expr a
  Var :: Int -> Expr a
  Let :: Expr a -> (a -> Expr a) -> Expr a
  (:+:) :: Expr a -> Expr a -> Expr a 
  (:-:) :: Expr a -> Expr a -> Expr a 
  (:*:) :: Expr a -> Expr a -> Expr a 
  (:/:) :: Expr a -> Expr a -> Expr a
  (:^:) :: Expr a -> Expr a -> Expr a

instance Num (Expr a) where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)

instance Fractional (Expr a) where  
  (/) = (:/:)

instance Floating (Expr a) where  
  (**) = (:^:)

eval :: Floating a => Env a -> Expr a -> a
eval e s = case s of
  Const c -> c
  Var i -> lookupEnv0 i e
  Let expr f -> let e' = eval e expr
                in eval e (f e') 
  a :+: b -> eval e a + eval e b
  a :-: b -> eval e a - eval e b
  a :*: b -> eval e a * eval e b
  a :/: b -> eval e a / eval e b
  a :^: b -> eval e a ** eval e b





let_ e1 e2 = Let e1 (e2 . Const)

one :: Num a => Expr a 
one = Const 1

treeE :: (Eq a, Num a) => a -> Expr a
treeE 0 = one
treeE n = let_(treeE (n-1)) (\s -> s :+: s)

env0 :: Env Double
env0 = fromListEnv [(0, 0.0)]









-- fun with recursion schemes

data Fix f = In (f (Fix f))

unFix :: Fix t -> t (Fix t)
unFix (In f) = f

para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para psi (In ff) = psi (keepCopy <$> ff) where
  keepCopy x = (x, para psi x)

para0 :: (a -> ([a], b) -> b) -> b -> [a] -> b
para0 psi z (x:xs) = psi x (xs, para0 psi z xs)
para0 _   z []     = z

paraL :: Foldable t => (t1 -> [t1] -> c -> c) -> c -> t t1 -> c
paraL  c n = snd . foldr (\ x (xs, t) -> (x : xs, c x xs t)) ([], n)

-- para' psi = snd . foldr (\ fxt -> (In (fst <$> fxt), psi fxt))




kata, kata' :: Functor f => (f b -> b) -> Fix f -> b
kata alg ff = alg . fmap (kata alg) $ unFix ff

kata' alg ff = alg . fmap (kata' alg) $ unFix ff
