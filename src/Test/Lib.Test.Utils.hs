{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Utils(genThree,arbitraryThree,arbitraryEnum,readShowProp,showReadShowProp) where

import Test.QuickCheck
import Dep.Structures(Three(..))
import Dep.Printing
import Data.Word
import Debug.Trace

genThree :: Gen a -> Int -> Gen (Three a)
genThree ge = gt
    where gt 0 = do
              vl <- ge
              return $ ThLeaf vl
          gt mx = do
              t <- arbitrary :: Gen Int
              gtt $ t `mod` 3
              where mx1 = mx-1
                    gtt 0 = gt 0
                    gtt 1 = do
                        vla <- gt mx1
                        vlb <- gt mx1
                        return $ ThNode vla vlb
                    gtt 2 = do
                        vl <- gt mx1
                        return $ ThDirect vl

arbitraryThree :: Arbitrary a => Int -> Gen (Three a)
arbitraryThree = genThree arbitrary

arbitraryEnum :: Enum a => a -> a -> Gen a
arbitraryEnum a0 a1 = do
     x <- arbitrary :: Gen Int
     return $ toEnum $ m0+(mod x (m1-m0+1))
     where m0 = fromEnum a0
           m1 = fromEnum a1

readShowProp :: (Eq a,Read a,Show a) => a -> Bool
readShowProp x = x == rs
    where sh = show x
          rs = read sh

showReadShowProp :: (Eq a,Read a,Show a) => a -> Bool
showReadShowProp x = x == rs && sh == sr
    where sh = show x
          rs = read sh
          sr = show rs
