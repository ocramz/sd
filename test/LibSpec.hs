module LibSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Numeric.SD

import qualified Data.IntMap.Strict as IM



main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Numeric.SD; f(x, y) = 3x^2+y^3" $ do
    it "d_x f(x,y) == 6x" $ 
      grad env0 e0 IM.! 0 `shouldBe` (Const 6.0 :*: Var 0)
    it "d_y f(x,y) == 3y^2" $ 
      grad env0 e0 IM.! 1 `shouldBe` (Const 3.0 :*: (Var 1 :^: Const 2))
    it "grad f(x,y) in (5,2) == (30, 12)" $
      evalGrad env0 e0 `shouldBe` IM.fromList [(0, 30.0), (1, 12.0)]
    -- it "works" $ do
    --   True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



-- | test data

env0 :: Env Double
env0 = Env $ IM.fromList [(0, 5.0), (1, 2.0)] -- x = 5, y = 2

e0, e1 :: Expr Double
e0 = Const 3 :*: (Var 0 :^: Const 2) :+: (Var 1 :^: Const 3)  --3x^2+y^3

e1 = Const 3 :*: (Var 1 :^: Const 2)  -- 3y^2

g0 = grad env0 e0   -- 6x, 3y^2

g0e = eval env0 <$> g0


-- g0 = eval env0 <$> grad env0 e0
