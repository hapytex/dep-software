{-# LANGUAGE FlexibleInstances #-}


import Dep.Structures(Reduceable(..),Three(..),BitTh(F,T),BitThSeq,Bitwise(..))

import Test.QuickCheck
import Test.Utils(showReadShowProp,arbitraryEnum)

instance Arbitrary BitTh where
    arbitrary = arbitraryEnum F T

main = do
   quickCheck (showReadShowProp :: BitTh -> Bool)
   quickCheck (showReadShowProp :: BitThSeq -> Bool)
