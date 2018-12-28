{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module that shows the waveheads of the signals during a simulation. It allows the user to edit the input wires and see the effect on the output immediately.
module Dep.Ui.WaveheadViewer (
        simulator
    ) where

import Control.Monad(mapM)

import Data.Bits
import qualified Data.BitVector as BV
import Data.IORef(newIORef,readIORef)
import Data.List
import qualified Data.Text as T
import Data.Word(Word16,Word8)

import Debug.Trace

import Graphics.Vty.Image
import Graphics.Vty.Input.Events
import Graphics.Vty.Prelude
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Events

import Dep.Algorithms.Sim(CircuitBuilder,startSimulation,horizon,getNamedWires)
import Dep.Structures
import Dep.Ui.Utils(KeyContextHandler(..),swapAttr,Decorator(..),UiDecorator(..),handleKeyWidget,linC,WidgetKeyHandling(..))
import Dep.Ui.Utils.Scrollable(autoScrollable,alwaysScrollable)
import Dep.Utils(selectBool,selN,spread0h,steps0)

import System.Exit(exitSuccess)

data WvhdSm = WvhdSm {wvhd :: CircuitBuilder (), t :: Int, line :: Int} deriving Show

type BVc = BV.BitVector

instance KeyContextHandler WvhdSm a where
    handleKeyCtx KRight _ c s = Just $ s {t = ts+1}
        where ts = t s
    handleKeyCtx KLeft  _ c s | ts > 0 = Just $ s {t = ts-1}
        where ts = t s
    handleKeyCtx KUp    _ c s | ls > 0 = Just $ s {line = ls-1}
        where ls = line s
    handleKeyCtx KDown  _ c s | ls < l1 = Just $ s {line = ls}
        where ls = 1+line s
              l1 = 0 -- TODO: length $ getNamedWires $ wvhd s
    handleKeyCtx _      _ _ _ = Nothing

instance WidgetKeyHandling WvhdSm

displayWaveheads :: Widget WvhdSm -> DisplayRegion -> RenderContext -> IO Image
displayWaveheads wg d@(dw,_) c = do
    foc <- (<~) focused wg
    WvhdSm cb cx cy <- getState wg
    let hz = horizon 0 dw (startSimulation cb) in return $ dispWh na nc (selectBool foc nf nfa) nfa cy dw cx hz
    where na = getNormalAttr c
          nf = Attr (attrStyle na) (SetTo (ISOColor 4)) (SetTo (ISOColor 0))
          nc = Attr (attrStyle na) (SetTo (ISOColor 0)) (SetTo (ISOColor 7))
          nfa = Attr (attrStyle na) (SetTo (ISOColor 1)) (SetTo (ISOColor 0))

dispWh :: Attr -> Attr -> Attr -> Attr -> Int -> Int -> Int -> Wavehead -> Image
dispWh atr0 atr1 atrf atrfa cy dw cx wh = foldr1 mgr (map (\(i,nm) -> dispWhI atr0 atr1 w1 nm <|> rndl i) $ zip [0..] $ wireNames wh) <-> mdl <-> (string atr0 (replicate w2 ' ') <|> markLine atr0 (dw-w2) 10 10)
    where w1 = (1+) $ maximum $ map length $ wireNames wh
          w2 = w1+1
          hc = linC 68
          cc = linC 85
          mdl = string atr0 $ take dw $ replicate (w1+1) hc ++ cycle (cc : replicate 9 hc)
          mgr x y = x <-> mdl <-> y
          cntl = take (dw-w2) $ steps0 $ spread0h $ wireStates wh
          rndl i = dispa (dispVi True) cx cntl i 0 <-> dispa dispMd cx cntl i 0 <-> dispa (dispVi False) cx cntl i 0
              where dispa = dispWhVl atr0 $ atrfi i
          atrfi i | i == cy = atrf
                  | otherwise = atrfa

markLine :: Attr -> Int -> Int -> Int -> Image
markLine attr w dn dv = ml 0
    where ml i | w < i = emptyImage
               | otherwise = string attr (take (w-i) (shi++replicate (dn-length shi) ' ')) <|> ml (i+dn)
              where shi = show $ div i dv

dispWhI :: Attr -> Attr -> Int -> String -> Image
dispWhI atr0 atr1 w1 st  = spc '1' <-> (string atr0 (st++replicate (w1-length st) ' ') <|> char atr1 (linC 17)) <-> spc '0'
    where spc x = string atr0 (replicate w1 ' ') <|> char atr1 x

dispVi :: Bool -> Bool -> Bool -> Char
dispVi False False True  = linC 65
dispVi False True  False = linC 5
dispVi True  False True  = linC 20
dispVi True  True  False = linC 80
dispVi cur   old   _     | old /= cur = ' '
                         | otherwise = linC 68

dispMd old new | old /= new = linC 17
               | otherwise = ' '

dispWhVl :: Attr -> Attr -> (Bool -> Bool -> Char) -> Int -> [(BVc,BVc)] -> Int -> Int -> Image
dispWhVl at af f sl ((xa,xb):xs) s x = char atu (f (testBit xa s) (testBit xb s)) <|> tl
    where tl = dispWhVl at af f sl xs s (x+1)
          atu | sl == x = af
              | otherwise = at
dispWhVl _  _  _ _  []           _ _ = emptyImage

simulator :: CircuitBuilder () -> IO (Widget WvhdSm)
simulator w = newWidget (WvhdSm w 0 0) $ \x -> x {
        render_ = displayWaveheads,
        keyEventHandler = handleKeyWidget,
        getCursorPosition_ = const $ return Nothing
    }
