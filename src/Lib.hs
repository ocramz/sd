{-# language DeriveFunctor #-}
module Lib where


import qualified Data.IntMap.Strict as IM
import Data.Maybe


-- | Binding environment; variables are indexed by integers
newtype Env a = Env { unEnv :: IM.IntMap a} deriving (Eq, Functor)
instance Show a => Show (Env a) where show = show . IM.toList . unEnv

-- newtype X = X { unX :: Int } deriving (Eq, Show)


lookupEnv0 :: Num e => Int -> Env e -> e
lookupEnv0 i (Env e) = fromMaybe 0 (IM.lookup i e)

fromListEnv :: [(Int, a)] -> Env a
fromListEnv = Env . IM.fromList



-- | Show integer-labeled variables as consecutive letters starting from 'x'
showVar :: Int -> String
showVar i = [v !! i] where
  v = cycle (['x' .. 'z'] ++ ['a' .. 'z'])
