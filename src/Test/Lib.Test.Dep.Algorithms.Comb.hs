import Test.QuickCheck
import Data.Maybe(fromJust)
import Dep.Structures(BitTh(..),BitThSeq,CombTable(..),CombFunc(..),Three(..),NmapM(..),BitLookup(..),CombElem(..),Assignable(..))
import Dep.Algorithms()
import Dep.Algorithms.Comb(synthetizeFun)
import Test.Utils(genThree)
import Data.Word
import Debug.Trace

prop_ct_id_eq :: CombTable -> Bool
prop_ct_id_eq t = t == t

prop_cf_id_eq :: CombFunc -> Bool
prop_cf_id_eq f = f == f

prop_synth_eq :: CombFunc -> Bool
prop_synth_eq cf = all (\x -> canAssignFrom (fromJust (btlookupTh x cf :: Maybe BitTh)) (fromJust (btlookupTh x synt :: Maybe BitTh))) (nseq $ xsize cf)
    where synt = synthetizeFun cf

prop_test_lookup_empty :: [BitTh] -> Bool
prop_test_lookup_empty x = btlookupTh x (SOP []) == Just F

prop_test_lookup_sempty :: [BitTh] -> Bool
prop_test_lookup_sempty x = btlookupTh x (SOP [[]]) == Just T

prop_test_lookup_minterm :: [Bool] -> [Bool] -> Bool
prop_test_lookup_minterm x y | length x == length y = btlookupTh (map b2t y) (SOP [bi2ti x]) == Just (b2t (x == y))
                             | otherwise = True

b2t :: Bool -> BitTh
b2t True = T
b2t _ = F

bi2ti :: [Bool] -> [Int]
bi2ti = f' 1
    where f' i (x:xs) | x = i : tl
                      | otherwise = (-i) : tl
                      where tl = f' (i+1) xs
          f' _ [] = []

nseq :: Int -> [BitThSeq]
nseq 0 = []
nseq 1 = [[T],[F]]
nseq n = [ F : ssi | ssi <- ss ] ++ [ T : ssi | ssi <- ss ]
    where ss = nseq (n-1)

instance Show CombFunc where
    show cf@(CF (CT n t) k) = show k++"/"++show t ++ " | "++(show $ synthetizeFun cf)

instance Arbitrary BitTh where
    arbitrary = do
        a <- arbitrary
        return $ toEnum $ a `mod` 3

instance Arbitrary CombTable where
    arbitrary = do
        n <- arbitrary
        m <- arbitrary
        tr <- genThree (vector (1 + m `mod` 8)) $ 2 + n `mod` 8
        return $ CT (2 + n `mod` 8) tr

instance Arbitrary CombFunc where
    arbitrary = do
        ct@(CT n _) <- arbitrary
        k <- arbitrary
        return $ CF ct (1+mod k (ysize ct))

main = do
    quickCheck prop_ct_id_eq
    quickCheck prop_cf_id_eq
    quickCheck prop_test_lookup_empty
    quickCheck prop_test_lookup_sempty
    quickCheck prop_test_lookup_minterm
    quickCheck prop_synth_eq
