{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dep.Ui.Utils.Border (
    border,edgeBorder
    ) where

import Control.Monad((>=>))

import Data.IORef(readIORef)

import Dep.Utils(selectBool)
import Dep.Ui.Utils(EdgeEmit(..),EdgeWidget(..),defaultRenderEdges,branchW,branchH,branchWinit,RenderEmitter,propagateShiftSetCurrentPosition,linC)

import Graphics.Vty.Attributes(Attr(..))
import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,string,char,imageWidth,imageHeight,crop,(<|>),(<->))
import Graphics.Vty.Input.Events(Key(..),Modifier)
import Graphics.Vty.Widgets.All(Widget(),newWidget,WidgetImpl(..),RenderContext(..),getNormalAttr,getState,updateWidgetState,addToFocusGroup)
import Graphics.Vty.Widgets.Core(handleKeyEvent,render,getCursorPosition,getCurrentSize,setCurrentPosition,growHorizontal,growVertical,relayFocusEvents,updateWidgetState,(<~~),(<~))
import Graphics.Vty.Widgets.Box(IndividualPolicy(..),BoxError(..))
import Graphics.Vty.Widgets.Util(plusWidth,plusHeight,withWidth,withHeight)

data BorderSt a = BorderSt { title :: String, innerWidget :: Widget a, redges :: RenderEmitter a }

instance Show (BorderSt a) where
    show bs = "[Border \""++title bs++"\"]"

showbrd :: Show a => Widget (BorderSt a) -> DisplayRegion -> RenderContext -> IO Image
showbrd wg dr@(dw,dh) rc = do
    bs <- getState wg
    wgi <- readIORef wg
    (im,ee) <- redges bs (innerWidget bs) (dw-2,dh-2) rc
    foc <- (<~) focused wg
    chFoc <- (<~) focused $ innerWidget bs
    return $ sbrd (normalAttr rc) (selectBool (foc || chFoc) (focusAttr rc) (normalAttr rc)) dr (title bs) im ee

sbrd :: Attr -> Attr -> DisplayRegion -> String -> Image -> EdgeEmit -> Image
sbrd atr atf (w,h) tl im ee = (ctl <|> crop (wi-2) 1 hst <|> ctr) <-> (vstl <|> im <|> vstr) <-> gsb
    where iw = imageWidth im+2
          ih = imageHeight im+2
          wi = min w iw
          hi = min h ih
          wdt = wi-4-length tl
          whl = div wdt 2
          eet = emitTop ee
          cht = char atr
          ctl = cht $ linC 20
          cbl = cht $ linC 5
          cbr = cht $ linC 65
          ctr = cht $ linC 80
          cto = cht $ linC 81
          ctc = cht $ linC 21
          bww = branchW atr
          hst | not (null tl) = bww whl [] eet <|> cto <|> string atf tl <|> ctc <|> branchWinit (wi-2-wdt+whl) atr (wi-2) [] eet
              | otherwise = bww (wi-2) [] eet
          gsb = cbl <|> bww (wi-2) (emitBottom ee) [] <|> cbr
          vst = branchH atr (hi-2)
          vstl = vst [] (emitLeft ee)
          vstr = vst (emitRight ee) []

mvcur :: Maybe (Int,Int) -> Maybe (Int,Int)
mvcur (Just (x,y)) = Just (x+1,y+1)
mvcur _            = Nothing

genBorder :: Show a => BorderSt a -> IO (Widget (BorderSt a))
genBorder st = do
    wRef <- newWidget st $ \w ->
        w {
            growHorizontal_ = growHorizontal . innerWidget,
            growVertical_ = growVertical . innerWidget,
            render_ = showbrd,
            keyEventHandler = \w k m -> do
                bs <- getState w
                handleKeyEvent (innerWidget bs) k m,
            getCursorPosition_ = getState >=> getCursorPosition . innerWidget >=> return . mvcur,
            setCurrentPosition_ = propagateShiftSetCurrentPosition (const $ return (1,1)) (return . innerWidget)
        }
    relayFocusEvents wRef $ innerWidget st
    return wRef

border :: Show a => Widget a -> String -> IO (Widget (BorderSt a))
border iw ti = genBorder $ BorderSt ti iw defaultRenderEdges

edgeBorder :: (Show a,EdgeWidget a) => Widget a -> String -> IO (Widget (BorderSt a))
edgeBorder iw ti = genBorder $ BorderSt ti iw renderEdges
