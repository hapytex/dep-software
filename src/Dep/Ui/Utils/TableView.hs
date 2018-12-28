{-# LANGUAGE TemplateHaskell #-}

module Dep.Ui.Utils.TableView (
        textTable,textTable'
    ) where

import Control.Monad((>=>))

import Data.IORef(readIORef)
import Data.List(transpose,map)

import Dep.Ui.Utils(EdgeEmit(..),EdgeWidget(..),linC)
import Dep.Utils(mapN,Table(..),powerl)

import Graphics.Vty.Attributes(Attr(..))
import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,string,char,imageWidth,imageHeight,pad,(<|>),(<->),emptyImage)
import Graphics.Vty.Input.Events(Key(..),Modifier)
import Graphics.Vty.Widgets.All(Widget(),newWidget,WidgetImpl(..),RenderContext(..),getNormalAttr,getState,updateWidgetState,addToFocusGroup)
import Graphics.Vty.Widgets.Core(handleKeyEvent,render,getCursorPosition,getCurrentSize,setCurrentPosition,growHorizontal,growVertical,relayFocusEvents,updateWidgetState,(<~~),(<~))
import Graphics.Vty.Widgets.Box(IndividualPolicy(..),BoxError(..))
import Graphics.Vty.Widgets.Util(plusWidth,plusHeight,withWidth,withHeight)

textTable' :: [[String]] -> [Int] -> IO (Widget Table)
textTable' dt hs = textTable $ Table dt hs [] -- TODO: add vs

textTable :: Table -> IO (Widget Table)
textTable st =
    newWidget st $ \w ->
        w {
            growHorizontal_ = const $ return False,
            growVertical_ = const $ return False,
            render_ = renderTable,
            keyEventHandler = \_ _ _ -> return False,
            getCursorPosition_ = const $ return Nothing,
            setCurrentPosition_ = \_ _ -> return ()
        }

renderTable :: Widget Table -> DisplayRegion -> RenderContext -> IO Image
renderTable w d r = do
    Table dt hl _ <- getState w -- TODO: take vertical lines into account
    return $ renderTbl d r ($(mapN 2) (string na) dt) hl
    where na = normalAttr r

renderTbl :: DisplayRegion -> RenderContext -> [[Image]] -> [Int] -> Image
renderTbl dr rc ims = rt 0 hhs $ padImages wws hhs ims
    where rt i hs     rs     (j:js) | i == j = hl <-> rt i hs rs js
          rt i (h:hs) (r:rs) js     = rti (vl na h) r <-> rt (i+1) hs rs js
              where rti _  [c] = c
                    rti br (c:cs) = c <|> br <|> rti br cs
                    rti _  []     = emptyImage
          rt i []     []     _      = emptyImage
          na = normalAttr rc
          (wws,hhs) = getImageDimensions ims
          hl = hline na wws

vl :: Attr -> Int -> Image
vl a = powerl emptyImage (char a $ linC 17) (<|>)

hline :: Attr -> [Int] -> Image
hline na = hl
    where hl [d] = bar d
          hl (d:ds) = bar d <|> char na (linC 85) <|> hl ds
          hl [] = emptyImage
          bar = powerl emptyImage (char na $ linC 68) (<|>)

padImages :: [Int] -> [Int] -> [[Image]] -> [[Image]]
padImages ww = padi
     where padi (h:hs) (is:iss) = padij h ww is : padi hs iss
           padi _      _        = []
           padij h (w:ws) (i:is) = pad 0 0 (w-imageWidth i) (h-imageHeight i) i : padij h ws is
           padij _ _      _      = []

getImageDimensions :: [[Image]] -> ([Int],[Int])
getImageDimensions dis = (map (maximum . map imageWidth) (transpose dis),
                          map (maximum . map imageHeight) dis)


