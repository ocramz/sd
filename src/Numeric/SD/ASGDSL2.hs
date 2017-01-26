{-# language RankNTypes #-}
module Numeric.SD.ASGDSL2 where

-- | a DSL with sharing, encoded with parametric higher order syntax (an ADT with a type parameter and Let constructors)


-- | NB: the functions in Let1 and Let2 can case-analyze their inputs
data Expr a = Const a
            | Var Int
            | Let1 (Expr a) (Expr a -> Expr a)
            | Let2 (Expr a) (Expr a) (Expr a -> Expr a -> Expr a)
            | (Expr a) :+: (Expr a)
            | (Expr a) :*: (Expr a)

showE (Const c)= show c
showE (Var i) = "v"++show i
showE (a :+: b) = unwords["(",showE a,"+",showE b, ")"]
showE (Let1 e f) = "(let"


test0 = Var 1 :+: Const 2

instance Show a => Show (Expr a) where show = showE


-- text e = go e 0 where
--   go One _ = "1"
--   go (Add e1 e2) c =
--    "("++go e1 c++" + "++go e2 c++")"
--   go (Var x) _ = x
--   go (Let e1 e2) c =
--    "(let "++v++" = "++go e1 (c + 1) ++
--    " in "++go (e2 v) (c + 1) ++")"
--      where v = "v"++show c



-- data Expr a = Const a
--             | Var Int
--             | Let1 (Expr a) (a -> Expr a)
--             | Let2 (Expr a) (Expr a) (a -> a -> Expr a)
--             | (Expr a) :+: (Expr a)
--             | (Expr a) :*: (Expr a)

-- let1 :: Expr a -> (Expr a -> Expr a) -> Expr a
-- let1 e f = Let1 e (f . Const)

-- let2 :: Expr a -> Expr a -> (Expr a -> Expr a -> Expr a) -> Expr a
-- let2 e1 e2 f = Let2 e1 e2 (\x y -> f (Const x) (Const y))

-- -- sim :: Expr a -> Expr a
-- sim ex = case ex of
--   (Const a :+: Const b) -> Const (a + b)
--   (Const a :+: Const 0) -> Const a
--   (Const 0 :+: Const a) -> Const a    
--   (a :+: b) -> let2 (sim a) (sim b) (:+:)
