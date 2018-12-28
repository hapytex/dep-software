{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | A module that defines widgets that print the schematics (gates and wires) for a given formula.
module Dep.Ui.Schematics (
        schematicsWidget,
        schematicsTick,
        sopWidget,
    ) where

import Control.Monad(mapM)

import Data.Bits
import Data.Bool(bool)
import Data.Function(on)
import Data.Hashable(Hashable())
import qualified Data.HashMap.Strict as HM
import Data.IORef(IORef(),readIORef)
import Data.List
import qualified Data.Text as T
import Data.Word(Word8)

import Debug.Trace

import Graphics.Vty.Attributes(MaybeDefault(SetTo),Color(ISOColor))
import Graphics.Vty.Image(Image(..),Attr(..),char,string,charFill,(<|>),(<->),emptyImage,imageWidth,imageHeight,crop)
import Graphics.Vty.Input.Events
import Graphics.Vty.Prelude
import Graphics.Vty.Widgets.All(newWidget,WidgetImpl(..),Widget(..),RenderContext(..),getState)
import Graphics.Vty.Widgets.Events

import Dep.Algorithms(calcSop)
import Dep.Printing
import Dep.Structures
import Dep.Ui.Utils(KeyContextHandler(..),handleKeyWidget,swapAttr,Decorator(..),UiDecorator(..),imageReplicate,WidgetKeyHandling(..),taplines,inboxH,mapHImg,calcRoutImg,lineLabel,vlineILabel,linC,shiftCursorWithPosition,wireAttr,highlightWireAttr)
import Dep.Ui.Utils.Scrollable(autoScrollable,ScrollSt())
import Dep.Utils(replicateFoldl1,ordNub,hashItemIndex,mapN,hashGenerator,withf)
import Dep.Utils.IORefFun(IORefFun(),readReference)

instance WidgetKeyHandling Int

instance KeyContextHandler Int a where
    handleKeyCtx _ _ _ _ = Nothing

data Schmtc inr a = Schmtc { cx :: Int, cy :: Int, schematic :: IORefFun inr a }

instance Show (Schmtc inr a) where
    show x = "[Schematic "++show (cx x,cy x)++"]"

instance WidgetKeyHandling (Schmtc inr [CombElem])

instance KeyContextHandler (Schmtc inr a) b where
    handleKeyCtx KLeft  []      _ s = Just $ s { cx = cx s-1}
    handleKeyCtx KRight []      _ s = Just $ s { cx = cx s+1}
    handleKeyCtx KUp    []      _ s = Just $ s { cy = cy s-1}
    handleKeyCtx KDown  []      _ s = Just $ s { cy = cy s+1}
    handleKeyCtx _      _       _ _ = Nothing

schmGcp :: Widget (Schmtc inr a) -> IO (Maybe DisplayRegion)
schmGcp wg = do
    st <- getState wg
    shiftCursorWithPosition wg $ Just (cx st, cy st)

displaySop :: Widget (Schmtc inr [CombElem]) -> DisplayRegion -> RenderContext -> IO Image
displaySop w d c = do
    sopRef <- getState w
    sops <- readReference $ schematic sopRef
    let (ima,oPosO) = (dispSop d c $ map (\(SOP x) -> x) sops) in return $ ima <-> vlineILabel norm (imageWidth ima) oPosO
    where norm = normalAttr c

dispSop :: DisplayRegion -> RenderContext -> [[[Int]]] -> (Image,[(Int,String,Attr)])
dispSop d@(iw,ih) c ts = ((tap_layer <|> lbl_layer) <-> neg_layer <-> and_layer <-> ano_layer <-> orr_layer,oPosO)
    where norm = normalAttr c
          wana x = highlightWireAttr norm (calcValue1 (<0) x) x
          wano x = highlightWireAttr norm (calcValue2 (<0) x) x
          ihf = inboxH (<|>) norm
          al = nub $ concat ts
          ol = map sort $ indexInvert al ts
          xs = xused al
          aIn@(imgc,aPos) = ihf $ map (\ai -> (wana ai,ai)) al
          oIn@(imgd,oPos) = ihf $ map (\oi -> (wireAttr norm oi,oi)) ol
          aPosC = concat aPos
          aPosO = map (\x -> snd (head x) + div (length x-1) 2) aPos
          oPosC = concat oPos
          oPosCI = map (\((_,x),y) -> (x,y)) oPosC
          oPosX = map (\x -> snd (head x) + div (length x-1) 2) oPos
          oPosO = zipWith (\x y -> (x,'f':show y,norm)) oPosX [0..]
          wdth = on max (maximum . map snd) aPosC oPosCI
          typN i = bool (ai '0') (ai '\x2502') (0 <= i)
                 where ai = char $ wana [abs i]
          gtU n = linC 40:replicate n (linC 137)++[linC 160]
          gtM c n = linC 34:replicate n c++[linC 34]
          gtL n = linC 10:replicate dn (linC 136)++linC 152:replicate (n-dn-1) (linC 136)++[linC 130]
              where dn = div (n-1) 2
          tap_layer = taplines norm wdth (wireInvert wana 0 aPosC)
          lbl_layer = lineLabel $ wireLabel wana 'x' $ nub $ map (abs . fst) aPosC
          neg_layer = mapHImg norm typN aPosC
          and_layer = imgc '\x2502' gtU <-> imgc '\x2502' (gtM '&') <-> imgc '\x2502' gtL
          ano_layer = calcRoutImg norm wdth $ wireMatch wana aPosO oPosC
          orr_layer = imgd '\x2502' gtU <-> imgd '\x2502' (gtM '|') <-> imgd '\x2502' gtL

calcValue1 :: (Int -> Bool) -> [Int] -> Bool
calcValue1 = all

calcValue2 :: (Int -> Bool) -> [[Int]] -> Bool
calcValue2 f = any $ calcValue1 f

wireInvert :: ([Int] -> Attr) -> Int -> [(Int,Int)] -> [([Int],Attr)]
wireInvert lut i ((a,b):xs) = (b:map snd pa, lut [aba]) : wireInvert lut (i+1) pb
    where aba = abs a
          (pa,pb) = partition ((aba ==) . abs . fst) xs
wireInvert _ _ [] = []

wireLabel :: ([Int] -> Attr) -> Char -> [Int] -> [(String,Attr)]
wireLabel lut c (a:as) = (c:show (aba-1),lut [aba]):wireLabel lut c as
    where aba = abs a
wireLabel  _  _ []     = []

wireMatch :: ([Int] -> Attr) -> [Int] -> [(([Int],Int),Int)] -> [([Int],Attr)]
wireMatch lut = wm 0
    where wm i (xa:aps) ops = (xa:map snd fli,lut flih) : wm (i+1) aps flj
              where (fli@(((flih,_),_):_),flj) = partition ((i ==) . snd . fst) ops
          wm _ [] _ = []

indexInvert :: (Hashable a,Eq a) => [a] -> [[a]] -> [[(a,Int)]]
indexInvert ixf = $(mapN 2) $ withf $ (HM.!) (hashItemIndex 0 ixf HM.empty)

attrWireHash :: Attr -> (Int -> Bool) -> [[[Int]]] -> HM.HashMap [Int] Attr
attrWireHash (Attr aa _ ac) lu xs = hashGenerator (\i a -> Attr (hli a) (SetTo $ ISOColor $ cola a i) ac) 0 hs HM.empty
    where fs = nub $ concat xs
          gs = map (:[]) $ concat fs
          hs = nub $ gs++fs
          hl = SetTo 32
          col True = cycle [3,6,5]
          col False = cycle [1,2,4]
          cola as = (!!) (cycle (col $ all lu as))
          hli as = if all lu as then hl else aa

xused :: [[Int]] -> [Int]
xused = ordNub . sort . concatMap (map abs)

-- | Construct a `Widget` that renders the given list of combinatorial elements by using an AND array and OR array.
sopWidget :: IORefFun inr [CombElem] -- ^ The given reference to a list of combinatorial elements.
    -> IO (Widget (ScrollSt (Schmtc inr [CombElem]))) -- ^ The returning widget that renders the combinatorial elements as a SOP and does this inside an automatic scroll element.
sopWidget c = do
    wid <- newWidget (Schmtc 0 0 c) $ \x -> x {
        growHorizontal_ = const $ return False,
        growVertical_ = const $ return False,
        render_ = displaySop,
        keyEventHandler = handleKeyWidget,
        getCursorPosition_ = schmGcp
    }
    autoScrollable wid

-- | A Widget that diesplays the given schematics and allows the user to edit the schematics accordingly.
schematicsWidget :: [Int] -- ^ The given schematics that must be rendered.
    -> IO (Widget (ScrollSt (Decorator [Int] UiDecorator))) -- ^ The returning widget: a Scrollable with the widget renderer internally.
schematicsWidget ct = do
    wid <- newWidget (Decorator ct [CursorX 0 3 0,CursorY 0 3 0]) $ \x -> x {
        growHorizontal_ = const $ return True,
        growVertical_ = const $ return True,
        render_ = displaySchematics,
        keyEventHandler = handleKeyWidget,
        getCursorPosition_ = const $ return Nothing
    }
    autoScrollable wid

displaySchematics :: Widget (Decorator [Int] UiDecorator) -> DisplayRegion -> RenderContext -> IO Image
displaySchematics w d c = return $ imageReplicate (crossMask norm) d
    where norm = normalAttr c

-- | A constant that defines the number of spaces between two ticks for the schematics editor, must be even.
schematicsTick :: Int -- ^ The number of spaces between two ticks for the schematics editor, must be even.
schematicsTick = 2

crossMask :: Attr -> Image
crossMask na = hl <-> (sd <|> st) <-> hl
    where n = schematicsTick
          st = char na '\x253c'
          sp = char na ' '
          sd = replicateFoldl1 (<|>) n sp
          md = replicateFoldl1 (<->) n sd
          hl = md <|> sp <|> md
