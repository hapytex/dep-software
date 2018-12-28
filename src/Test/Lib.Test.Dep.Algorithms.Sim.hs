import Test.QuickCheck
import Dep.Algorithms.Sim(CircuitBuilder, newWire, newNamedWire)

prop_inc_circuit = True

instance Arbitrary (CiruitBuilder ()) where
    arbitrary = do
        c <- arbitrary
        s <- arbitrary
        return $ f (mod c 1) s
            where f 0 Nothing = newWire >> return ()
                  f 0 (Just x) = newNamedWire x >> return ()

instance Arbitrary Circuit where
    arbitrary = return emptyCircuit

main = 
    quickCheck prop_inc_circuit
