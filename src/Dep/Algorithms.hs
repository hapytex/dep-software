{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dep.Algorithms (
        expandTh,leaves,theeLeaves,pathLeaves,pathSeq,                -- functions
        mapPathThree,
        calcSop,
    ) where

-- A package for algorithms used in almost any chapter on dep-oriented data structures.

import Data.Array(listArray)
import qualified Data.Array as Ar

import Dep.Structures (
        Three(..),BitTh(..),FSMState(..),FSM(..),CombElem(..),
        CombTable(..),CombFunc(..),                                             -- datas
        BitThSeq,                                                               -- types
        Reduceable(..),BitLookup(..),Bitwise(..),Specifiable(..),VarInv(..),    -- classes
        Equivalent(..),EqBy(..),NmapM(..),Assignable(..),
    )
import Dep.Utils(commutative)

import Debug.Trace(trace)

-- | Check if for the given lookup function and the sum-of-products, the sop is True or False.
calcSop :: (a -> Bool) -- ^ The given lookup function.
    -> [[a]] -- ^ The given sum-of-products.
    -> Bool -- ^ A boolean indicating whether the sum-of-products is True, False otherwise.
calcSop lu = any (all lu)

instance Assignable BitTh where
    canAssignTo _ D = True
    canAssignTo x y = x == y
    canAssignFrom D _ = True
    canAssignFrom x y = x == y

instance Eq a => Assignable (FSMState a) where
    canAssignTo _ DontCare = True
    canAssignTo x y = x == y
    canAssignFrom DontCare _ = True
    canAssignFrom x y = x == y

instance Assignable [BitTh] where
    canAssignTo (x:xs) (y:ys) = canAssignTo x y && canAssignTo xs ys
    canAssignTo (_:_) [] = False
    canAssignTo [] (_:_) = False
    canAssignTo [] [] = True
    canAssignFrom (x:xs) (y:ys) = canAssignFrom x y && canAssignFrom xs ys
    canAssignFrom (_:_) [] = False
    canAssignFrom [] (_:_) = False
    canAssignFrom [] [] = True

instance Equivalent [Int] BitThSeq where
    eqto = eqto' 1
        where eqto' i (a:as) | i == a  = T : tl
                             | i == (-a) = F : tl
                             | otherwise = replicate (aba-i) D ++ eqto' aba as
                  where tl = eqto' (i+1) as
                        aba = abs a
              eqto' _ [] = []
    eqfrom = eqfr' 1
        where eqfr' i (b:bs) | b == D = tl
                             | b == T = i : tl
                             | otherwise = (-i) : tl
                  where tl = eqfr' (i+1) bs
              eqfr' _ [] = []

instance Eq a => Eq (Three a) where
    (==) = eqby (==)

instance EqBy Three where
    eqby eqf = tst
        where tst (ThLeaf a) (ThLeaf b) = eqf a b
              tst l@(ThLeaf _) (ThDirect m) = tst l m
              tst (ThDirect m) l@(ThLeaf _) = tst m l
              tst l@(ThLeaf _) (ThNode ma mb) = tst l ma && tst l mb
              tst (ThNode ma mb) l@(ThLeaf _) = tst ma l && tst mb l
              tst (ThDirect l) (ThDirect m) = tst l m
              tst (ThNode la lb) (ThNode ma mb) = tst la ma && tst lb mb
              tst (ThNode la lb) (ThDirect m) = tst la m && tst lb m
              tst (ThDirect m) (ThNode la lb) = tst m la && tst m lb

instance Eq CombTable where
    (==) (CT aa ab) (CT ba bb) = aa == ba && ab == bb

instance Eq CombFunc where
    (==) (CF aa ab) (CF ba bb) = aa == ba && ab == bb

instance Eq a => Reduceable (Three a) where
    reduce (ThNode la lb) | ra == rb = reduce (ThDirect ra)                     -- use reduced variants to save some work
                          | otherwise = ThNode ra rb                            -- reduce leafes and try to reduce oneself
        where ra = reduce la
              rb = reduce lb
    reduce (ThDirect dl) | ThLeaf _ <- rel = rel                                -- pull leafs up
                         | otherwise = ThDirect rel
        where rel = reduce dl
    reduce x = x                                                                -- fallback: don't reduce

--utility methods
expandTh :: Int -> Three a -> Three a
expandTh n (ThLeaf a) = expandThTail n a
    where expandThTail :: Int -> a -> Three a
          expandThTail n x | n == 0 = ThLeaf x
                           | otherwise = ThNode xn1 xn1
              where xn1 = expandThTail (n-1) x
expandTh n (ThNode a b) = ThNode (expandTh n1 a) (expandTh n1 b) where n1 = n-1
expandTh n (ThDirect a) = ThNode ea ea where ea = expandTh (n-1) a

instance (Eq a) => BitLookup (Three a) a where
    btlookupTh _ (ThLeaf x) = Just x
    btlookupTh (_:bs) (ThDirect x) = btlookupTh bs x
    btlookupTh (D:bs) (ThNode la lb) = maybeEqMerge (btlookupTh bs la) (btlookupTh bs lb)
    btlookupTh (T:bs) (ThNode _ lb) = btlookupTh bs lb
    btlookupTh (F:bs) (ThNode la _) = btlookupTh bs la

instance BitLookup CombTable BitThSeq where
    btlookupTh ad (CT _ t) = btlookupTh ad t

instance BitLookup CombFunc BitTh where
    btlookupTh ad (CF t k) | Just y <- btlookupTh ad t = Just $ y !! (k-1)
                           | otherwise = Nothing
    btlookupTh ad (CFS _ t _) = btlookupTh ad t

maybeEqMerge :: Eq a => Maybe a -> Maybe a -> Maybe a
maybeEqMerge (Just x) (Just y) | x == y = Just x
                               | otherwise = Nothing
maybeEqMerge Nothing _ = Nothing
maybeEqMerge _ Nothing = Nothing

instance (Eq ix,Num ix) => Specifiable [a] ix a where
    specifyModMay i0 f = spm i0
        where spm 0 l@(x:xs) | Just y <- f x = y : xs
                             | otherwise = l
              spm i (x:xs) = x : spm (i-1) xs
              spm _ [] = []
    specify i0 v = spm i0
        where spm 0 (_:xs) = v : xs
              spm i (_:xs) = spm (i-1) xs
              spm 0 [] = [v]
              spm _ [] = []

instance Functor Three where
    fmap f = fmp
        where fmp (ThLeaf x) = ThLeaf $ f x
              fmp (ThDirect x) = ThDirect $ fmp x
              fmp (ThNode xa xb) = ThNode (fmp xa) (fmp xb)

instance Eq a => Specifiable (Three a) BitThSeq a where
    specify arg fvl = reduce.treespy (const $ ThLeaf fvl) arg
    specifyMod idx f = reduce.treespy (fmap f) idx

treespy :: (Three a -> Three a) -> BitThSeq -> Three a -> Three a
treespy fun = ts
    where ts (b:bs) (ThNode la lb) = ThNode (sftSpy F la) (sftSpy T lb)
              where sftSpy d l | canAssign b d = ts bs l
                               | otherwise = l
          ts (D:bs) (ThDirect l) = ts bs l
          ts (b:bs) (ThDirect l) = ThNode (inj F) (inj T)
              where inj c | b == c = ts bs l
                          | otherwise = l
          ts (D:bs) l@(ThLeaf _) = ThDirect (ts bs l)
          ts (b:bs) l@(ThLeaf _) = ThNode (sftSpy F) (sftSpy T)
              where sftSpy d | canAssign b d = ts bs l
                             | otherwise = l
          ts [] t0 = fun t0

instance Specifiable CombTable BitThSeq BitThSeq where
    specify idx vl (CT n tr) = CT n (specify idx vl tr) --TODO: check for length of vl first?
    specifyMod idx fun (CT n tr) = CT n (specifyMod idx fun tr) --TODO: encapsulate fun such that length is maintained?

instance Specifiable CombFunc BitThSeq BitTh where
    specify idx vl (CF tr k) = CF (specifyMod idx ((specify::Int->BitTh->BitThSeq->BitThSeq) (k-1) vl) tr) k
    specify idx vl (CFS n tr k) = CFS n (specify idx vl tr) k
    specifyMod idx fun (CF tr k) = CF (specifyMod idx ((specifyMod::Int->(BitTh->BitTh)->BitThSeq->BitThSeq) (k-1) fun) tr) k
    specifyMod idx fun (CFS n tr k) = CFS n (specifyMod idx fun tr) k

instance Bitwise BitTh where
    neg F = T
    neg T = F
    neg _ = D
    (.&) F _ = F
    (.&) T x = x
    (.&) _ F = F
    (.&) _ _ = D
    (.|) T _ = T
    (.|) F x = x
    (.|) _ T = T
    (.|) _ _ = D
    (.^) F x = x
    (.^) T x = neg x
    (.^) _ _ = D

instance BitLookup CombElem BitTh where
    btlookupTh bts (MinT its) = Just $ foldr1 (.&) $ select its 1 bts
    btlookupTh bts (MaxT its) = Just $ foldr1 (.|) $ select its 1 bts
    btlookupTh bts l@SOP {} = Just $ foldr ((.|) . tl) F $ terms l
        where lb = length bts
              lut = listArray (-lb,lb) $ reverse (map neg bts) ++ D : bts
              tl = foldr ((.&) . (Ar.!) lut) T

select :: [Int] -> Int -> [BitTh] -> [BitTh]
select [] _ _ = []
select _ _ [] = [D]
select (i:is) o (b:bs) | o == -i = neg b : t
                       | o == i = b : t
                       | otherwise = select (i:is) ai $ drop (ai-o-1) bs
                       where ai = abs i
                             t = select is (o+1) bs

leaves :: Three a -> [a]
leaves = lvs []

pathLeaves :: Three a -> [(a,[Int])]
pathLeaves = plvs [] [] 1

lvs :: [a] -> Three a -> [a]
lvs tl (ThLeaf v) = v : tl
lvs tl (ThDirect t) = lvs tl t
lvs tl (ThNode ta tb) = lvs (lvs tl tb) ta

plvs :: [(a,[Int])] -> [Int] -> Int -> Three a -> [(a,[Int])]
plvs tl h _ (ThLeaf v) = (v,h) : tl
plvs tl h i (ThDirect t) = plvs tl h (i+1) t
plvs tl h i (ThNode ta tb) = plvs (plvs tl (i:h) i1 tb) ((-i):h) i1 ta
    where i1 = i+1

theeLeaves :: [BitTh] -> Three a -> [a]                                         -- Tribute to Sybill Trelawney
theeLeaves = tlvs []
    where tlvs tl [] t = lvs tl t
          tlvs tl _ (ThLeaf v) = v : tl
          tlvs tl (_:xs) (ThDirect t) = tlvs tl xs t
          tlvs tl (F:xs) (ThNode ta _) = tlvs tl xs ta
          tlvs tl (T:xs) (ThNode _ tb) = tlvs tl xs tb
          tlvs tl (_:xs) (ThNode ta tb) = tlvs (tlvs tl xs tb) xs ta

instance NmapM CombTable where
    xsize (CT n _) = n
    ysize (CT _ t) = length $ head $ leaves t

instance NmapM CombFunc where
    xsize (CF t _) = xsize t
    xsize (CFS n _ _) = n
    ysize _ = 1

mapPathThree :: (Eq a) => Int -> (a -> a) -> [Int] -> Three a -> Three a
mapPathThree _ f [] t = fmap f t
mapPathThree i f l lv@(ThLeaf x) | x == fx = lv
                                 | otherwise = specifyPath lv fx i l
    where fx = f x
mapPathThree i f xl@(x:xs) (ThDirect t) | i /= ax = ThDirect $ mpt xl t
                                        | otherwise = ThDirect $ mpt xs t
    where ax = abs x
          mpt = mapPathThree (i+1) f
mapPathThree i f xl@(x:xs) (ThNode tl tr) | i /= ax = ThNode (mpt xl tl) (mpt xl tr)
                                          | x <= 0 = ThNode (mpt xs tl) tr
                                          | otherwise = ThNode tl $ mpt xs tr
    where ax = abs x
          mpt = mapPathThree (i+1) f

specifyPath :: Three a -> a -> Int -> [Int] -> Three a
specifyPath _ v _ [] = ThLeaf v
specifyPath d v i (x:xs) | ax /= i = ThDirect $ sp (x:xs)
                         | x <= 0 = ThNode (sp xs) d
                         | otherwise = ThNode d $ sp xs
                         where ax = abs x
                               sp = specifyPath d v (i+1)

pathSeq :: Three a -> [Int] -> [a]
pathSeq = pathSeq' 1

pathSeq' :: Int -> Three a -> [Int] -> [a]
pathSeq' _ l [] = leaves l
pathSeq' _ (ThLeaf a) _ = [a]
pathSeq' i (ThDirect r) l = pathSeq' (i+1) r l
pathSeq' i (ThNode rl rr) (li:ls) | ali < i = pathSeq' i (ThNode rl rr) ls
                                  | ali > i = pathSeq' iic rl lis++pathSeq' iic rr lis
                                  | li < 0 = pathSeq' iic rl lis
                                  | otherwise = pathSeq' iic rr lis
                                  where ali = abs li
                                        iic = i+1
                                        lis = li:ls

instance VarInv CombElem where
    involvedVar (MinT vs) = map abs vs
    involvedVar (MaxT vs) = map abs vs
    involvedVar (SOP vs) = map abs $ foldl1 mergeVindex vs
    involvedVar (POS vs) = map abs $ foldl1 mergeVindex vs

mergeVindex :: [Int] -> [Int] -> [Int]
mergeVindex (x:xs) (y:ys) | ax < ay = ax : mergeVindex xs (y:ys)
                          | ax > ay = ay : mergeVindex (x:xs) (y:ys)
                          | otherwise = ax : mergeVindex xs ys
                          where ax = abs x
                                ay = abs y
mergeVindex [] ys = ys
mergeVindex xs [] = xs
