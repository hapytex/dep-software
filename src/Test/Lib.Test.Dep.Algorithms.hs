{-# LANGUAGE FlexibleInstances #-}

import Dep.Algorithms()
import Dep.Structures(Reduceable(..),Three(..),BitTh(..),Bitwise(..))

import Test.Utils(genThree)

import Test.QuickCheck

prop_id_eq :: Eq a => Three a -> Bool
prop_id_eq t = t == t

prop_id_reduc :: Eq a => Three a -> Bool
prop_id_reduc t = t == reduce t

prop_bt_eq :: (Bool -> Bool -> Bool) -> (BitTh -> BitTh -> BitTh) -> Bool -> Bool -> Bool
prop_bt_eq f g x y  = mapbt (f x y) == g (mapbt x) (mapbt y)

prop_eq_and :: Bool -> Bool -> Bool
prop_eq_and = prop_bt_eq (&&) (.&)

prop_eq_or :: Bool -> Bool -> Bool
prop_eq_or = prop_bt_eq (||) (.|)

--prop_eq_xor :: Bool -> Bool -> Bool
--prop_eq_xor = prop_bt_eq (^) (.^)

mapbt :: Bool -> BitTh
mapbt True = T
mapbt _ = F

instance Arbitrary (Three Int) where
    arbitrary = do
        n <- arbitrary
        genThree genInt $ 2+(n `mod` 8)

genInt :: Gen Int
genInt = do
    n <- arbitrary
    return $ n `mod` 10

main = do
   quickCheck (prop_id_reduc :: Three Int -> Bool)
   quickCheck (prop_id_eq :: Three Int -> Bool)
   quickCheck prop_eq_and
   quickCheck prop_eq_or
--   quickCheck prop_eq_xor
