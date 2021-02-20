{-# LANGUAGE TemplateHaskell #-}

module Dep.Ui.Utils.TableView (
        textTable,textTable'
    ) where

import Brick.Types(Context, RenderM, Result(Result, image), Size(Fixed), Widget(Widget), attrL, emptyResult, getContext)

import Control.Monad((>=>))
import Control.Lens((^.))

import Data.IORef(readIORef)
import Data.List(transpose,map)

import Dep.Ui.Utils(EdgeEmit(..), availRegionL, linC)
import Dep.Utils(mapN,Table(..),powerl)

import Graphics.Vty.Attributes(Attr(..))
import Graphics.Vty.Image(DisplayRegion, Image, char, emptyImage, imageHeight, imageWidth, pad, string, (<|>), (<->))

textTable' :: [[String]] -> [Int] -> Widget a
textTable' dt hs = textTable $ Table dt hs [] -- TODO: add vs

textTable :: Table -> Widget a
textTable st = Widget Fixed Fixed (renderTable st)

renderTable :: Table -> RenderM a (Result n)
renderTable (Table dt hl _) = do  -- TODO: take vertical lines into account
    ctx <- getContext
    let na = ctx ^. attrL
    let d = ctx ^. availRegionL
    return $ emptyResult { image=renderTbl d ctx ($(mapN 2) (string na) dt) hl }

renderTbl :: DisplayRegion -> Context -> [[Image]] -> [Int] -> Image
renderTbl dr rc ims = rt 0 hhs $ padImages wws hhs ims
    where rt i hs     rs     (j:js) | i == j = hl <-> rt i hs rs js
          rt i (h:hs) (r:rs) js     = rti (vl na h) r <-> rt (i+1) hs rs js
              where rti _  [c] = c
                    rti br (c:cs) = c <|> br <|> rti br cs
                    rti _  []     = emptyImage
          rt i []     []     _      = emptyImage
          na = rc ^. attrL
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


