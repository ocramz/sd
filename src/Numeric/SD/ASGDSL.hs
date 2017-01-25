{-# language RankNTypes #-}
module Numeric.SD.ASGDSL where

data Expr a = One
            | Add (Expr a) (Expr a)
            | Var a
            | Let (Expr a) (a -> Expr a)

let_ :: Expr a -> (Expr a -> Expr a) -> Expr a
let_ e1 e2 = Let e1 (e2 . Var)

eval :: Num t => Expr t -> t
eval One = 1
eval (Add a b) = eval a + eval b
eval (Var x) = x
eval (Let e f) = eval (f (eval e))


type ClosedExpr = forall a . Expr a


text :: ClosedExpr -> String
text e = go e 0 where
  go One _ = "1"
  go (Add e1 e2) c =
   "("++go e1 c++" + "++go e2 c++")"
  go (Var x) _ = x
  go (Let e1 e2) c =
   "(let "++v++" = "++go e1 (c + 1) ++
   " in "++go (e2 v) (c + 1) ++")"
     where v = "v"++show c

treeE :: Int -> Expr a
treeE 0 = One
treeE n = let_ (treeE (n - 1)) (\shared -> Add shared shared)

-- | example:
-- > eval (treeE 20)
