import Test.QuickCheck
import Dep.Printing.Schematics
import Data.Word
import Debug.Trace

instance Arbitrary Direction where
    arbitrary = do
        a <- arbitrary
        return $ toEnum $ a `mod` 4

instance Arbitrary Wire where
    arbitrary = do
        w <- arbitrary
        return $ maskWire (mod w 16)

prop_id_mwwm :: Wire -> Bool
prop_id_mwwm x = x == (maskWire $ wireMask x)

prop_id_wmmw :: Word8 -> Bool
prop_id_wmmw x = (x `mod` 16) == ((wireMask $ maskWire $ x `mod` 16) `mod` 16)

main = do
    quickCheck prop_id_mwwm
    quickCheck prop_id_wmmw
