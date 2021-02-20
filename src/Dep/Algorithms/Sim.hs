{-# LANGUAGE FlexibleInstances #-}

-- | A module to building and simulate circuits. Building circuits is done using the CircuitBuilder monad.
module Dep.Algorithms.Sim (
    Wire(),  -- we do not expose wire, to avoid extracting/manipulating wires
    Gate(),  -- we do not expose gate, to avoid extracting/manipulating gates
    Circuit(),
--    ) where
) where

import Control.Monad.State.Lazy

import qualified Data.Sequence as Sq

import Dep.Utils(nand, nor)

type Ticks = Int

newtype Wire = Wire { wireId :: Int } deriving (Eq, Ord)

data Gate = Gate { gateWire :: Maybe Wire, sensors :: [Wire], template :: GateTemplate, gateFanIn :: Int }
data GateTemplate = GateTemplate { gateFunction :: [Bool] -> Bool, gateDelay :: Int -> Int -> Ticks }
newtype Circuit = Circuit { circuitGates :: Sq.Seq [Gate] }
type CircuitState = State Circuit

moveWire_ :: Int -> Wire -> Wire
moveWire_ d (Wire a) = Wire (d+a)

moveGateWire_ :: Int -> Gate -> Gate
moveGateWire_ d g@Gate{gateWire=j} = g {gateWire=fmap (moveWire_ d) j}

instance Semigroup Circuit where
    Circuit ca <> Circuit cb = Circuit (ca Sq.>< cb')
        where shft = Sq.length ca
              cb' | shft > 0 = fmap (fmap (moveGateWire_ shft)) cb
                  | otherwise = cb

instance Monoid Circuit where
    mempty = Circuit Sq.empty

gateDelay_ :: Int -> Int -> Int -> Int -> Ticks
gateDelay_ s d n = const (d + s * n)

normalDelay :: Int -> Int -> Ticks
normalDelay = gateDelay_ 4 16

invertDelay :: Int -> Int -> Ticks
invertDelay = gateDelay_ 4 6

andGate :: GateTemplate
andGate = GateTemplate and normalDelay

orGate :: GateTemplate
orGate = GateTemplate or normalDelay

nandGate :: GateTemplate
nandGate = GateTemplate nand invertDelay

norGate :: GateTemplate
norGate = GateTemplate nor invertDelay

notGate :: GateTemplate
notGate = GateTemplate (not . head) (const (const 10))

createGate :: GateTemplate -> [Wire] -> Gate
createGate g ws = Gate Nothing ws g (length ws)

addSensor_ :: Gate -> Wire -> Sq.Seq [Gate] -> Sq.Seq [Gate]
addSensor_ g (Wire w) = Sq.adjust (g:) w

removeSensor_ :: Wire -> [Gate] -> [Gate]
removeSensor_ w = filter ((Just w /=) . gateWire)

addWire_ :: [Gate] -> CircuitState Wire
addWire_ gs = do
    c@(Circuit cg) <- get
    put (c { circuitGates=(Sq.|>) cg gs})
    return (Wire (Sq.length cg))

addWire :: CircuitState Wire
addWire = addWire_ []

addGate :: Gate -> CircuitState Wire
addGate g = do
    w <- addWire
    let g' = g { gateWire=Just w }
    c@(Circuit cg) <- get
    put (c {circuitGates=foldr (addSensor_ g') cg (sensors g')})
    return w

stripWire :: Wire -> CircuitState ()
stripWire w = do
    c@(Circuit cg) <- get
    put (c {circuitGates=fmap (removeSensor_ w) cg})

replaceWire :: Gate -> Wire -> CircuitState ()
replaceWire g w = do
    stripWire w
    let g' = g {gateWire=Just w}
    c@(Circuit cg) <- get
    put (c {circuitGates=foldr (addSensor_ g') cg (sensors g')})
