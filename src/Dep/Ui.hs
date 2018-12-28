{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module that binds handles interactive communication with the user through a text-based user interface.
-- The module and its submodules use the `Graphics.Vty` package.
module Dep.Ui (
        runShowKarnaugh,runSynthetize,
        runMinimizeFsm,
        clearAndExit
    ) where

import Control.Monad(mapM)

import Data.Bits
import Data.IORef(newIORef,readIORef)
import Data.List
import qualified Data.Text as T
import Data.Word(Word16,Word8)

import Debug.Trace

import Graphics.Vty.Image
import Graphics.Vty.Input.Events
import Graphics.Vty.Prelude
import Graphics.Vty.Widgets.All(addToCollection,addToFocusGroup,defaultContext,FocusGroup,mergeFocusGroups,newCollection,newFocusGroup,onKeyPressed,runUi,vBox,Widget)
import Graphics.Vty.Widgets.Events

import Dep.Algorithms.Comb(synthetize)
import Dep.Algorithms.Sim()
import Dep.Printing(tableFSM)
import Dep.Structures
import Dep.Tables.Parser()
import Dep.Ui.CpuViewer(cpuViewer)
import Dep.Ui.Karnaugh(karnaughWidget)
import Dep.Ui.Schematics(schematicsWidget,sopWidget)
import Dep.Ui.Utils(KeyContextHandler(..),swapAttr,Decorator(..),UiDecorator(..),handleKeyWidget)
import Dep.Ui.Utils.Boxes(hBoxes,setBoxesSpacing)
import Dep.Ui.Utils.BinaryEditor(bined)
import Dep.Ui.Utils.Border(border,edgeBorder)
import Dep.Ui.Utils.Scrollable(autoScrollable,alwaysScrollable)
import Dep.Ui.Utils.TableView(textTable')
import Dep.Ui.WaveheadViewer(simulator)
import Dep.Utils(selectBool,Table())
import Dep.Utils.IORefFun(toRefFunSnd)

import System.Console.ANSI(clearScreen,setCursorPosition)
import System.Exit(exitSuccess)

instance Show Table where
    show _ = ""

whSample = undefined

-- | Clear the screen, set the cursor at the top left of the screen and exit the program successfully. This is done when exiting the program, for instance when the user presses <kbd>Ctrl</kbd>+<kbd>Q</kbd>.
clearAndExit :: IO Bool -- ^ The IO monad that handles the request to exit, always returns True.
clearAndExit = setCursorPosition 0 0 >> clearScreen >> exitSuccess >> return True

-- | Add Ctrl+Q to the focus group and run the user interface with the given widget as top widget.
runUiFg :: Show a => Widget a -- ^ The given top control widget.
    -> Widget FocusGroup -- ^ The given focus group widget.
    -> IO () -- The I/O monad that ensures the application is up and running.
runUiFg ui fg = do
    c <- newCollection
    _ <- addToCollection c ui fg
    onKeyPressed fg $ \_ key mod -> if key == KChar 'q' && mod == [MCtrl] then clearAndExit else return False
    runUi c defaultContext

-- | Run the user interface and show Karnaugh cards as well as an implementation using primimplicants.
runShowKarnaugh :: String -- ^ The given query to process, ignored here.
    -> String -- ^ The given input to process, here the combinatorial table.
    -> IO () -- ^ The resulting IO monad because interaction is not lazy.
runShowKarnaugh qr np = do
    fg0 <- newFocusGroup
    (w0,fg1) <- karnaughWidget kk
    fg <- mergeFocusGroups fg0 fg1
    syn <- newIORef (kk,synthetize kk)
    w1 <- sopWidget $ toRefFunSnd syn
    addToFocusGroup fg w1
    b0 <- border w0 "Karnaugh cards"
    b1 <- border w1 "Schematics"
    ui <- vBox b0 b1
    runUiFg ui fg
        where kk = read np
--TODO: autoupdate

-- | Run the user interface and show how a combinatorial table is converted into an implementation using gates (by using Karnaugh cards with primimplicants).
runSynthetize :: String -- ^ The given query to process, ignored here.
    -> String -- ^ The given input to process, here the combinatorial table.
    -> IO () -- ^ The resulting IO monad because interaction is not lazy.
runSynthetize = runShowKarnaugh

runMinimizeFsm :: String -> String -> IO ()
runMinimizeFsm _ ip = do
    fg <- newFocusGroup
    ui <- textTable' (tableFSM $ (read :: String -> FSM String BitThSeq BitThSeq) ip) [1]
    addToFocusGroup fg ui
    runUiFg ui fg

--}
{--
runVty qr np = do
    fg0 <- newFocusGroup
    (w0,fg1) <- cpuViewer
    fg <- mergeFocusGroups fg0 fg1
    c <- newCollection
    _ <- addToCollection c w0 fg
    onKeyPressed fg $ \_ key mod -> if key == KChar 'q' && mod == [MCtrl] then clearAndExit else return False
    runUi c defaultContext
--}
