{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A module to building and simulate circuits. Building circuits is done using the CircuitBuilder monad.
module Dep.Algorithms.Sim (
      CircuitBuilder,
      Wire(wireName), getWire, getWires, getNamedWires,
      CircuitState(),
      circuitState,
--    validName,                                                         -- validators
--    Wire(..),sensList,                                                 -- data structures
--    emptyCircuit,addWire,addWires,addNamedWire,                        -- circuit modifiers
--    getNamedWires,renameWire,pxRenameWires,getWireNames,               -- circuit getters
--    getWire,getWireName,isInputWire,isGateWire,
--    Circuit,CircuitBuilder,resultingCircuit,startSimulation,wires,     -- circuit building facilities
--    makeNotGate,makeAndGate,makeOrGate,makeNandGate,makeNorGate,       -- circuit gate introductions
--    makeClock,makeClock0,                                              -- circuit clock introductions
--    getCircuitNamedWires,                                              -- obtain data from the circuit builder
--
--    generateDefaultState,generateCustomState,                          -- circuit to circuit state functions
--    CircuitState,                                                      -- simulation data structures
--    initCustomSim,initDefaultSim,setTime,advanceTime,advanceTime1,     -- simulation modifiers
--    nextEventTime,
--
--    resultingCircuitState,                                             -- obtain the circuit state after simulation
--    scheduleInputWire,                                                 -- user interaction for simulation
--    getWireState,getTime,                                              -- obtain the state of the circuit state
      horizon,                                                           -- generate the waveheads for the given list of wires
      startSimulation
--    ) where
) where

import Control.Monad.State.Lazy(State, get, put, gets, execState)

import Data.Bits(Bits, xor, zeroBits)
import Data.Function(on)
import qualified Data.Heap as He
import Data.List(find, findIndices)
import Data.Maybe(isJust)
import qualified Data.Vector as Vc
import Data.Vector(snoc, (!), (!?), (//))

import Dep.Utils(Tabulatable(..), Table(..), listIndices)

type WireStatus = Bool
type Delay = Int
type WireEmit = (WireStatus, Delay)
type GateFunction = [(WireStatus,WireStatus)] -> Maybe WireEmit
type Time = Int

startSimulation = undefined

horizon = undefined

data GateT = GateT { gateFunc :: [WireStatus] -> WireStatus, gateAry :: Int, gateName :: String }
data Wire = Wire { wireIndex :: Int, wireName :: Maybe String, wireTag :: WireTag }
data WireEvent = WireEvent { wireEventTime :: Int,
                             wireEventIndex :: Int,
                             wireEventStatus :: WireStatus} deriving (Eq, Ord)
newtype Circuit = Circuit { wires :: Vc.Vector Wire }
data CircuitState = CircuitState { eventHeap :: EventHeap, circuitStateTime :: Time, circuit :: Circuit }
type EventHeap = He.MinHeap WireEvent

circuitState :: Circuit -> CircuitState
circuitState = CircuitState He.empty 0

data WireTag = EmptyTag
             | Gate { gateTemplate :: GateT, gateWires :: [Int]}

_newGate :: GateT -> [Wire] -> WireTag
_newGate t = Gate t . map wireIndex

printWireTag :: WireTag -> [String]
printWireTag EmptyTag = []
printWireTag Gate {gateTemplate=t, gateWires=ws} = gateName t : listIndices "" "x" ws

printWire :: Wire -> [String]
printWire Wire {wireIndex=i, wireName=n, wireTag=t} = [show i, maybe "" show n] ++ printWireTag t

instance Tabulatable Circuit where
    toTable cir = Table (map printWire (Vc.toList (wires cir))) [] [2,3]
    fromTable (Table cnt _ _) = execState (mapM_ go cnt) emptyCircuit
        where go [_,nm] = _newWire EmptyTag (mnm nm)
              go (_:nm:tm:ws) | Just t <- getTemplate tm = _newWire (_newGate t (map (\wi -> Wire wi Nothing EmptyTag) $ findIndices (/= "") ws)) (mnm nm)
                              | otherwise = error ("Can not find template \""++ tm ++"\"")
              go _ = error "Incorrectly formatted table."
              mnm "" = Nothing
              mnm x = Just x

emptyCircuit :: Circuit
emptyCircuit = Circuit Vc.empty

type CircuitBuilder = State Circuit

instance Show (CircuitBuilder ()) where
    show _ = "CircuitBuilder"

notGate :: GateT
notGate = GateT (not . head) 0 "not"

andGate :: GateT
andGate = GateT and 0 "and"

nandGate :: GateT
nandGate = GateT (not . and) 0 "nand"

orGate :: GateT
orGate = GateT or 0 "or"

norGate :: GateT
norGate = GateT or 0 "or"

allXor :: Bits a => [a] -> a
allXor = foldr xor zeroBits

xorGate :: GateT
xorGate = GateT allXor 0 "xor"

xnorGate :: GateT
xnorGate = GateT (not . allXor) 0 "xnor"

gateTemplates :: [GateT]
gateTemplates = [notGate, andGate, nandGate, orGate, norGate, xorGate, xnorGate]

getTemplate :: String -> Maybe GateT
getTemplate name = find ((name ==) . gateName) gateTemplates

_newWire :: WireTag -> Maybe String -> CircuitBuilder Wire
_newWire wt nm = do
    c@Circuit {wires=w} <- get
    let wi = Wire {wireIndex=Vc.length w, wireName=nm, wireTag=wt}
    put $ c {wires=snoc w wi}
    return wi

_replaceWire :: (Wire -> Wire) -> Wire -> CircuitBuilder Wire
_replaceWire f w = do
    c@Circuit {wires=ws} <- get
    let ix = wireIndex w
    let wi = (Vc.!) ws ix
    put c {wires=ws // [(ix, f wi)]}
    return wi

newWire :: CircuitBuilder Wire
newWire = _newWire EmptyTag Nothing

newNamedWire :: String -> CircuitBuilder Wire
newNamedWire = _newWire EmptyTag . Just

getWire :: Int -> CircuitBuilder (Maybe Wire)
getWire = gets . (. wires) . flip (Vc.!?)

getWires :: CircuitBuilder [Wire]
getWires = gets (Vc.toList . wires)

getNamedWires :: CircuitBuilder [Wire]
getNamedWires = gets (filter (isJust . wireName) . Vc.toList . wires)

getNumberOfWires :: CircuitBuilder Int
getNumberOfWires = gets (Vc.length . wires)

_addGate :: GateT -> [Wire] -> Maybe String -> CircuitBuilder Wire
_addGate temp ws = _newWire (_newGate temp ws)

addGate :: GateT -> [Wire] -> CircuitBuilder Wire
addGate temp ws = _addGate temp ws Nothing

renameWire :: String -> Wire -> CircuitBuilder Wire
renameWire nm = _replaceWire (\w -> w {wireName = Just nm})

addNamedGate :: GateT -> [Wire] -> String -> CircuitBuilder Wire
addNamedGate temp ws = _addGate temp ws . Just

makeGate :: GateT -> [Wire] -> Wire -> CircuitBuilder Wire
makeGate temp ws = _replaceWire (\w -> w {wireTag = _newGate temp ws})

type CircuitSimulation = State CircuitState

_advanceTime :: Time -> CircuitSimulation Time
_advanceTime dtm = do
    c <- get
    let tm = circuitStateTime c + dtm
    put c { circuitStateTime = tm }
    return tm
