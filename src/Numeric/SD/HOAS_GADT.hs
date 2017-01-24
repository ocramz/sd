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


