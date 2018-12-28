{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

-- | A module that allows to generate widgets that show (and allow editing of) the CPU state.
module Dep.Ui.CpuViewer (
    cpuViewer
    ) where

import Control.Arrow(first,second)
import Control.Monad(mapM,(>=>))

import Data.Bits(shiftL,shiftR)
import Data.IORef(readIORef)
import Data.List
import Data.Tuple(swap)
import qualified Data.Text as T
import Data.Word(Word16)

import Graphics.Vty.Image
import Graphics.Vty.Input.Events
import Graphics.Vty.Prelude
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Events

import Dep.Algorithms()
import Dep.Printing()
import Dep.Structures
import Dep.Ui.Utils(KeyContextHandler(..),swapAttr,Decorator(..),UiDecorator(..),alternate,matrixImg,composeImg,flattenImg,handleKeyWidget,WidgetKeyHandling(..),uiCurX,uiCurY,shiftCursorWithPosition)
import Dep.Ui.Utils.Border(border,edgeBorder)
import Dep.Ui.Utils.Boxes(hBoxes,setBoxesSpacing)
import Dep.Ui.Utils.Scrollable(autoScrollable)
import Dep.Ui.Utils.BinaryEditor(bined,bineds,scrollBineds)

import Dep.Utils(selectBool,burst,burstInner,burstItems,concatReplicate,(<&|>),(<&->))
import Dep.Algorithms.Comb(synthetizeFun)


--newCpuViewer :: IO ()
cpuViewer = do
    fg <- newFocusGroup
    w0 <- bined (1425 :: Word16)
    w1 <- bined (1028 :: Word16)
    w2 <- bineds [1..7 :: Word16]
    w3 <- bined (0 :: Word16)
    w4 <- scrollBineds [0..535 :: Word16]
    b0 <- border w0 "Instruction"
    b1 <- border w1 "Program Counter"
    b2 <- border w2 "Registers"
    b3 <- border w3 "Status"
    b4 <- border w4 "Memory"
    addToFocusGroup fg w0
    addToFocusGroup fg w1
    addToFocusGroup fg w2
    addToFocusGroup fg w3
    addToFocusGroup fg w4
    x0 <- vBox b0 b1
    x1 <- vBox x0 b2
    x2 <- vBox x1 b3
    x3 <- hBox x2 b4
    return (x3,fg)
