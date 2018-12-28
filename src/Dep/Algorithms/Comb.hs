{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dep.Algorithms.Comb (
                cacheCF,maximizeMin,synthetize,synthetizeFun                    -- functions
    ) where

-- A package for algorithms used in the "combinatorial circuits" chapter.

import Debug.Trace(trace)
import Data.Maybe
import Data.Monoid(Monoid(mempty, mappend))

import Dep.Algorithms (theeLeaves,expandTh,pathSeq,mapPathThree)
import Dep.Structures (
        Reduceable(..),Expandable(..),BitLookup(..),Mergeable(..),              -- classes
        NmapM(..),Assignable(..),
        CombTable(..),CombFunc(..),Three(..),BitTh(..),BitThSeq,                -- datas
        CombElem(..),
        Drawable(..),                                                           -- optionals (TODO: remove)
        sop                                                                     -- functions
    )
import Dep.Printing

-- cache a CombFun

cacheCF :: CombFunc -> CombFunc
cacheCF (CF (CT n t) i) = CFS n (reduce $ fmap (!! (i-1)) t) i
cacheCF (CFS n t i) = CFS n (reduce t) i

extractFi :: CombTable -> Int -> CombFunc
extractFi x = cacheCF . CF x

-- reduce combinatorial table

instance Reduceable CombTable where
    reduce (CT n t) = CT n (reduce t)

-- expand combinatorial table

instance Expandable CombTable where
    expand (CT n t) = CT n (expandTh n t)

-- Merging three bits with don't care and lists

instance Semigroup BitTh where
    (<>) D = id
    (<>) x = const x

instance Monoid BitTh where
    mempty = D

instance Mergeable BitTh where
    merge D x = Just x
    merge x D = Just x
    merge x y | x == y = Just x
              | otherwise = Nothing

instance Mergeable [BitTh] where
    merge (x:xs) (y:ys) | Just f <- merge x y = merge xs ys >>= Just . (f :)
                        | otherwise = Nothing
    merge [] ys = Just ys
    merge xs [] = Just xs

-- synthetize
synthetize :: CombTable -> [CombElem]
synthetize ct = map (synthetizeFun . extractFi ct) [1..(ysize ct)]

specialize :: Int -> Int -> [Int] -> [Int]
specialize i n [] = map negate [i..n]
specialize i n (x:xs) | i > n = []
                      | i == ax = x : sin xs
                      | otherwise = -i : sin (x:xs)
                      where ax = abs x
                            sin = specialize (i+1) n

synthetizeFun :: CombFunc -> CombElem
synthetizeFun (CFS n tr _) = sop $ synth tr
    where synth t | Just _ <- smt = so : synth tb
                  | otherwise = []
                  where smt = scrapeMin 1 t
                        sm = fromJust smt
                        lb = length sm
                        ss = specialize 1 n sm
                        (so,_) = maximizeMin t ss
                        tb = blankout so t
synthetizeFun l = synthetizeFun $ cacheCF l

scrapeMin :: Int -> Three BitTh -> Maybe [Int]
scrapeMin _ (ThLeaf T) = Just []
scrapeMin _ (ThLeaf _) = Nothing
scrapeMin n (ThDirect d) = scrapeMin (n+1) d
scrapeMin n (ThNode la lb) | Just ra <- smn1 la = Just (-n:ra)
                           | otherwise = smn1 lb >>= Just . (n:)
                           where smn1 = scrapeMin (n+1)

blankout :: [Int] -> Three BitTh -> Three BitTh
blankout a = reduce . rabl 1 a

rabl :: Int -> [Int] -> Three BitTh -> Three BitTh
rabl i xl@(x:xs) (ThNode tl tr) | i /= xa = ThNode (rabli xl tl) $ rabli xl tr
                                | x < 0 = ThNode (rabli xs tl) tr
                                | otherwise = ThNode tl $ rabli xs tr
    where xa = abs x
          rabli = rabl (i+1)
rabl i xl@(x:xs) (ThDirect d) | i /= xa = ThDirect $ rabli xl d
                              | x < 0 = ThNode (rax d) d
                              | otherwise = ThNode d $ rax d

    where xa = abs x
          rabli = rabl (i+1)
          rax = rabli xs
rabl i xl@(x:xs) vl@(ThLeaf v) | i /= xa = ThDirect $ rabli xl vl
                               | x < 0 = ThNode (rabli xs vl) vl
                               | otherwise = ThNode vl $ rabli xs vl
    where xa = abs x
          rabli = rabl (i+1)
rabl _ [] _ = ThLeaf D

maximizeMin :: Three BitTh -> [Int] -> ([Int],Int)
maximizeMin t x = minimizeMin' (pathSeq t) [] x (length x)

minimizeMin' :: ([Int] -> BitThSeq) -> [Int] -> [Int] -> Int -> ([Int],Int)
minimizeMin' l h [] w | validMin l h = (h,w)
                      | otherwise = (h,w+9999999)
minimizeMin' l h (x:xs) w | not $ validMin l (h++(x:xs)) = (h,w+9999999)
                          | wa <= wb = (ha,wa)
                          | otherwise = (hb,wb)
                          where (ha,wa) = minimizeMin' l h xs (w-1)
                                (hb,wb) = minimizeMin' l (h++[x]) xs w

validMin :: ([Int] -> BitThSeq) -> [Int] -> Bool
validMin f = all (canAssign T) . f
