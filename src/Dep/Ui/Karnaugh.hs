{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

-- | A module that allows to generate widgets that show (and allow editing of) Karnaugh cards.
module Dep.Ui.Karnaugh (
        karnaugh1Widget,karnaughWidget
    ) where

import Control.Arrow(first,second)
import Control.Monad(mapM,(>=>))

import Data.Bits(shiftL,shiftR)
import Data.IORef(readIORef)
import Data.List
import Data.Tuple(swap)
import qualified Data.Text as T

import Debug.Trace

import Graphics.Vty.Image
import Graphics.Vty.Input.Events
import Graphics.Vty.Prelude
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.Events

import Dep.Algorithms()
import Dep.Printing()
import Dep.Structures
import Dep.Ui.Utils(KeyContextHandler(..),swapAttr,Decorator(..),UiDecorator(..),alternate,matrixImg,composeImg,flattenImg,handleKeyWidget,WidgetKeyHandling(..),uiCurX,uiCurY,shiftCursorWithPosition)
import Dep.Ui.Utils.Boxes(hBoxes,setBoxesSpacing)
import Dep.Ui.Utils.Scrollable(autoScrollable)
import Dep.Utils(selectBool,burst,burstInner,burstItems,concatReplicate,(<&|>),(<&->))
import Dep.Algorithms.Comb(synthetizeFun)

type Inr a = a -> a
type Opr a = a -> a -> a
type Tu2 a = (a,a)
type Ta2 a = [Tu2 a]

instance KeyContextHandler CombFunc (Decorator CombFunc UiDecorator) where
    handleKeyCtx k m c fnc = Just $ specifyModMay pth tos fnc
        where dc = decorators c
              pth = cursmt 4 (uiCurX dc,uiCurY dc)
              tos = handleKeyCtx k m c :: BitTh -> Maybe BitTh

instance Show CombFunc where
    show (CF ct i) = show (cttab ct)++'/':show i

instance WidgetKeyHandling CombFunc

displayKarnaugh :: Widget (Decorator CombFunc UiDecorator) -> DisplayRegion -> RenderContext -> IO Image
displayKarnaugh w d c = do
    r <- readIORef w
    Decorator (CF (CT n tr) vi) (_:_:Option d _ _:_) <- getState w
    return $ dispK c d n tr vi r

dispK c d n tr vi r = (twig <|> kMark '\x251c' '\x2500' '\x2524' (<|>) (<->) norm nw 0) <-> (string norm (replicate (nwh-nh) ' ') <|> kMark '\x252c' '\x2502' '\x2534' (<->) (<|>) norm nh 1 <|> outerKgh norm d n vi tr)
    where nh = div n 2
          nw = n-nh
          nwh = max nw nh
          norm = normalAttr c
          twig = matrixImg twigyx norm nwh nwh
              where twigyx 0 x | x > 0 = (!!) (show (vi-1) ++ repeat ' ') (x-1) --TODO: fix show twig
                               | otherwise = 'f'
                    twigyx y x | y == x = '\\'
                               | otherwise = ' '

kMark :: Char -> Char -> Char -> Opr Image -> Opr Image -> Attr -> Int -> Int -> Image
kMark c0 c1 c2 fw fh a n0 = kMark0 n2
    where n2 = shiftL 1 n0
          w = 2*n2+1
          br s n i = foldl1 fw . map (char a) $ take w $ (++) (replicate s ' ') $ cycle $ burstInner c0 c1 c2 (show i) (n+1) ++ replicate (n-1) ' '
          kMark0 :: Int -> Int -> Image
          kMark0 n i = fh (br n n i) $ kMark1 (div n 2) $ i+2
          kMark1 :: Int -> Int -> Image
          kMark1 1 _ = emptyImage
          kMark1 n i = fh (br n (2*n) i) $ kMark1 (div n 2) $ i+2

outerKgh :: Attr -> Bool -> Int -> Int -> Three [BitTh] -> Image
outerKgh a@(Attr aa _ ac) c n v t = composeImg $ flattenImg $ lyr [(a,(:) tp $ zipWith (\i x -> li i : x ++ [ri i]) (cycle [False,True]) inr++[bt])]
    where inr = innerKgh n v t
          iw = length $ head inr
          tp = '\x250f' : take iw (cycle "\x2501\x252f") ++ "\x2513"
          bt = '\x2517' : take iw (cycle "\x2501\x2537") ++ "\x251b"
          li True = '\x2520'
          li _ = '\x2503'
          ri True = '\x2528'
          ri _ = '\x2503'
          lyr = selectBool c ((++) $ zip (map (\i -> Attr aa (SetTo $ ISOColor i) ac) (cycle [1..6])) $ map (uncurry (printRmb n) . rmb n) $ reverse $ terms $ synthetizeFun (CF (CT n t) v)) id --reverse is used to make small regions more visible

innerKgh :: Int -> Int -> Three [BitTh] -> [String]
innerKgh n0 v = dykghW n0
    where v1 = v-1
          dykghW = dykgh dykghH (zipWith3 (\j x -> (++) x . f j . reverse) (cycle [False,True]))
          f True = (:) '\x253c'
          f _    = (:) '\x2502'
          dykghH = dykgh dykghW (\x -> (++) x . (:) (take (length $ head x) $ cycle "\x2500\x253c") . reverse)
          dykgh :: (Int -> Three [BitTh] -> [String]) -> Opr [String] -> Int -> Three [BitTh] -> [String]
          dykgh f _ 0 (ThLeaf a) = [[show a !! v1]]
          dykgh f m n t = uncurry m $ decap t
              where fn = f (n-1)
                    decap :: Three [BitTh] -> ([String],[String])
                    decap l@(ThLeaf _) = (ls,ls)
                        where ls = fn l
                    decap (ThDirect l) = (ls,ls)
                        where ls = fn l
                    decap (ThNode la lb) = (fn la,fn lb)

karnaugh1Widget :: CombFunc -> IO (Widget (Decorator CombFunc UiDecorator))
karnaugh1Widget cf@(CF (CT n _) _) = newWidget (Decorator cf [CursorX 0 (shiftL 1 (div (n+1) 2)-1) 0,CursorY 0 (shiftL 1 (div n 2)-1) 0,Option True (KChar 'c') 0]) $ \x -> x {
        growHorizontal_ = const $ return False,
        growVertical_ = const $ return False,
        render_ = displayKarnaugh,
        keyEventHandler = handleKeyWidget,
        getCursorPosition_ = kCurPos
    }

kCurPos :: Widget (Decorator CombFunc UiDecorator) -> IO (Maybe (Int,Int))
kCurPos wg = do
    Decorator (CF (CT n _) _) (CursorX _ _ cx:CursorY _ _ cy:_) <- getState wg
    shiftCursorWithPosition wg $ let m = 1+div (n+1) 2 in Just (2*cx+m,2*cy+m)

karnaughWidget ct = do
    es <- mapM (karnaugh1Widget . CF ct) [1..ysize ct]
    (e,fg) <- hBoxes es
    setBoxesSpacing e 1
    w <- autoScrollable e
    return (w,fg)

rmb :: Int -> [Int] -> Tu2 (Ta2 Int)
rmb n = rmbW n 1
    where mgrmb :: Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
          mgrmb d [] ys = swaprmb (shiftL d 1) ys
          mgrmb _ xs [] = xs
          mgrmb d xs ys = init xs++f (last xs) ysmh
              where (ysmh:ysms) = swaprmb (shiftL d 1) ys
                    f (xa,dx) (ya,dy) | xa+dx >= ya = (xa,max dx $ ya+dy-xa) : ysms
                                      | otherwise = (xa,dx) : (ya,dy) : ysms
          swaprmb :: Int -> [(Int,Int)] -> [(Int,Int)]
          swaprmb d = swaprmb' []
              where swaprmb' :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
                    swaprmb' ts ((xa,dx):xs) = swaprmb' ((d-xa-dx,dx):ts) xs
                    swaprmb' ts _ = ts
          rmbW :: Int -> Int -> [Int] -> Tu2 (Ta2 Int)
          rmbW = rmbWH rmbH (\w d b -> ([(0,b)],[(d,w)])) snd second
          rmbH :: Int -> Int -> [Int] -> Tu2 (Ta2 Int)
          rmbH = rmbWH rmbW (\w d b -> ([(d,w)],[(0,b)])) fst first
          rmbWH :: (Int -> Int -> [Int] -> Tu2 (Ta2 Int)) -> (Int -> Int -> Int -> Tu2 (Ta2 Int)) -> (Tu2 (Ta2 Int) -> Ta2 Int) -> (Inr (Ta2 Int) -> Tu2 (Ta2 Int) -> Tu2 (Ta2 Int)) -> Int -> Int -> [Int] -> Tu2 (Ta2 Int)
          rmbWH f _ _ _ 0 _ _ = ([],[])
          rmbWH f _ s m n i xs@(x:xs2@(_:_)) | i == (-x) = irs
                                             | i == x = m (mgrmb w []) irs
                                             where w = shiftL 1 $ shiftR (n-1) 1
                                                   irf = f (n-1) (i+1)
                                                   irs = irf xs2
          rmbWH f g s m n i xs@(x:_) | i == (-x) = g w 0 b
                                     | i == x = g w w b
                                     where w = shiftL 1 $ shiftR (n-1) 1
                                           b = shiftL 1 $ shiftR n 1
          rmbWH f _ s m n i xs@(_:_) = m (mgrmb w $ s ir) ir
               where w = shiftL 1 $ shiftR (n-1) 1
                     ir = f (n-1) (i+1) xs
          rmbWH _ _ _ _ _ _ _      = ([],[])

printRmb :: Int -> Ta2 Int -> Ta2 Int -> [String] --TODO: modulo check
printRmb n ys xs = burstItems l0 l1 l2 l3 h (outlineG ys)
    where xsg = outlineG xs
          nh = shiftR n 1
          nw = n-nh
          w = 1+shiftL 2 nw
          h = 1+shiftL 2 nh
          l0 = replicate w ' '
          l1 = burstItems ' ' '\x250c' '\x2500' '\x2510' w xsg
          l2 = burstItems ' ' '\x2502' ' ' '\x2502' w xsg
          l3 = burstItems ' ' '\x2514' '\x2500' '\x2518' w xsg
          outlineG :: [(Int,Int)] -> [(Int,Int)]
          outlineG = map (\(x,y) -> (2*x,2*y+1))

cursmt :: Int -> (Int,Int) -> BitThSeq --TODO: optimize
cursmt = mt mth ntow fst first
    where ntow n = shiftL 1 $ shiftR (n+1) 1
          mth = mt cursmt ntow snd second
          mt _ _ _ _ 0 _ = []
          mt r w s m n c | sc >= hl = T : rin (m (const $ wn-sc-1) c)
                           | otherwise = F : rin c
              where sc = s c
                    wn = w n
                    hl = shiftR wn 1
                    rin = r (n-1)
