import Test.QuickCheck
import Dep.Ui.Schematics(schematicsTick)
import Data.Word
import Debug.Trace

prop_ticks_even :: Bool
prop_ticks_even = even schematicsTick

main = do
    quickCheck prop_ticks_even
