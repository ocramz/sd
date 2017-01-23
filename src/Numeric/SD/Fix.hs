{-# language DeriveFunctor #-}
module Numeric.SD.Fix where

import qualified Data.IntMap.Strict as IM

import Data.Fix (Fix, cata)
-- import Data.Reify


-- | A binding environment; variables are indexed by integers
newtype Env a = Env { unEnv :: IM.IntMap a} deriving (Eq, Show)

-- | Expression ADT
infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a = Const a
            | Var Int
            | a :+: a
            | a :-: a
            | a :*: a
            | a :/: a
            | a :^: a              
            deriving (Eq, Functor)

instance Show a => Show (Expr a) where
  show (Const x) = show x
  show (Var i) = showVar i
  show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (a :/: b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (a :-: b) = "(" ++ show a ++ " - " ++ show b ++ ")"  
  show (a :^: b) = show a ++ "^" ++ show b

-- | Show integer-labeled variables as consecutive letters starting from 'x'
showVar i = [v !! i] where
  v = cycle (['x' .. 'z'] ++ ['a' .. 'z'])



eval' e (Const c) = c
eval' (Env e) (Var x) = undefined

-- eval :: Env t -> Fix Expr -> a
eval e = cata (eval' e)
