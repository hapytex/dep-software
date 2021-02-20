module Dep.Algorithms.Seq (
    reduceFSM,overFSM,concatFSM,headFSM,concatReduceFSM,
    graySequence
) where

import Control.Applicative(Applicative((<*>),pure))

import qualified Data.BitVector as BV
import Data.Function (flip)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.List(intersperse,find)
import Data.Word(Word64)

import Debug.Trace(trace)

import Dep.Algorithms
import Dep.Structures
import Dep.Utils(fixpoint,singleGroupBy,grayInc,hashItemLists)

graySequence :: Int -> [BV.BitVector]
graySequence n = map (BV.bitVec n) $  0 : takeWhile (/= 0) (iterate gi (gi 0 :: Word64))
    where gi = grayInc n

instance Functor FSMState where
    fmap f (Label x) = Label (f x)
    fmap _ DontCare = DontCare

instance Applicative FSMState where
    pure = Label
    (<*>) (Label f) (Label x) = Label (f x)
    (<*>) _ _ = DontCare

instance Monad FSMState where
    return = Label
    (>>=) DontCare = const DontCare
    (>>=) (Label x) = ($ x)

instance Eq a => Mergeable (FSMState a) where
    merge la@(Label a) (Label b) | a == b = Just la
                                 | otherwise = Nothing
    merge DontCare x = Just x
    merge x DontCare = Just x


instance (Eq st,Hashable st,Ord ot) => Reduceable (FSM st it ot) where
    reduce = headFSM . reduceFSM

concatReduceFSM :: (Eq st,Hashable st,Ord ot) => FSM [st] it ot -> FSM [st] it ot
concatReduceFSM = concatFSM . reduceFSM


instance Functor (FSM a b) where
    fmap f m@Moore {emitMo=e} = m {emitMo=f.e}
    fmap f m@Mealy {emitMy=e} = m {emitMy=(f.).e}

overFSM :: (Hashable su, Eq su) => ([st] -> su) -> FSM [st] it ot -> FSM su it ot
overFSM g m@Moore {states=ss, transit=dt, emitMo=f} = m {states=ss', transit=(fmap g .) . dt . lut, emitMo=f . lut}
    where ss' = map g ss
          lut = lutss ss ss'
overFSM g m@Mealy {states=ss, transit=dt, emitMy=f} = m {states=ss', transit=(fmap g .) . dt . lut, emitMy=f . lut}
    where ss' = map g ss
          lut = lutss ss ss'

lutss :: (Eq a,Hashable a) => [[b]] -> [a] -> a -> [b]
lutss ss ss' = flip (HM.lookupDefault []) (HM.fromList $ zip ss' ss)

concatFSM :: (Hashable st,Eq st) => FSM [[st]] it ot -> FSM [st] it ot
concatFSM = overFSM concat

headFSM :: (Hashable st,Eq st) => FSM [st] it ot -> FSM st it ot
headFSM = overFSM head

reduceFSM :: (Hashable st,Eq st, Eq ot) => FSM st it ot -> FSM [st] it ot
reduceFSM (Moore ss is dt f) = Moore rs is (\x y -> pm $ dt (head x) y) $ f.head
    where rs = calcRedStates is dt (reduce1Moore ss f)
          pm = Label . flip (HM.lookupDefault []) (hashItemLists rs HM.empty) . stateLabel
reduceFSM (Mealy ss is dt f) = Mealy rs is (\x y -> pm $ dt (head x) y) (f . head)
    where rs = calcRedStates is dt (reduce1Mealy ss is f)
          pm = Label . flip (HM.lookupDefault []) (hashItemLists rs HM.empty) . stateLabel


calcRedStates :: (Eq st) => [it] -> (st -> it -> FSMState st) -> [[st]] -> [[st]]
calcRedStates is dt = fixpoint (reduceInc is dt)

flatStates :: [[FSMState st]] -> [FSMState [st]]
flatStates = map (Label . map stateLabel)

reduce1Moore :: (Eq ot) => [st] -> (st -> ot) -> [[st]]
reduce1Moore ss f = singleGroupBy f ss

reduce1Mealy :: (Eq ot) => [st] -> [it] -> (st -> it -> ot) -> [[st]]
reduce1Mealy ss ins f = singleGroupBy (\x -> map (f x) ins) ss

partitionHeader :: (Eq a) => [[a]] -> FSMState a -> FSMState a
partitionHeader _  DontCare = DontCare
partitionHeader ps (Label o) | Just pi <- find (elem o) ps = Label $ head pi
                             | otherwise = DontCare

reduceInc :: (Eq st) => [it] -> (st -> it -> FSMState st) -> [[st]] -> [[st]]
reduceInc is dt pt = concatMap (singleGroupBy (\s -> map (partitionHeader pt . dt s) is)) pt
