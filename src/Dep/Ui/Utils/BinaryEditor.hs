{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A module that provides widget to show and edit binary values. The values are usually represented both binary and as a hexadecimal number.
module Dep.Ui.Utils.BinaryEditor (
    BinEd(),bined,bineds,scrollBineds,getBinValue,setBinValue,
    ) where

import Control.Monad((>=>),liftM)

import Data.Array.Unboxed(UArray,IArray,elems,listArray,(!),(//),bounds)
import Data.Bits(Bits(),FiniteBits(finiteBitSize),testBit,(.&.),clearBit,setBit,complementBit,rotateL,rotateR,shiftR)
import Data.Char(chr)

import Graphics.Vty.Attributes(Attr(..),MaybeDefault(SetTo),Color(ISOColor))
import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,char,imageWidth,imageHeight,emptyImage,(<|>),(<->))
import Graphics.Vty.Input.Events(Key(..))
import Graphics.Vty.Widgets.All(Widget(),newWidget,WidgetImpl(..),RenderContext(..),getState,updateWidgetState)

import Dep.Ui.Utils(handleKeyWidget,WidgetKeyHandling(..),KeyContextHandler(..),EdgeWidget(..),EdgeEmit(..),shiftCursorWithPosition)
import Dep.Ui.Utils.Scrollable(autoScrollable)

-- | The datastructure that is used by the binary editor internally.
data BinEd a = (IArray UArray a,Bits a) => BinEd { cursor :: Int, value_ :: a, hex :: Bool } | BinEds { cursor :: Int, line_ :: Int, values_ :: UArray Int a, hex :: Bool }

line :: BinEd a -> Int
line b@BinEds{} = line_ b
line _ = 0

value :: IArray UArray a => BinEd a -> a
value b@BinEd{} = value_ b
value b@BinEds{} = values_ b!line_ b

values :: IArray UArray a => BinEd a -> [a]
values b@BinEd{} = [value_ b]
values b@BinEds{} = elems $ values_ b

instance Show a => Show (BinEd a) where
    show (BinEd c v _) = show v++'(':show c++")"
    show _ = "multiple registers"

instance (FiniteBits a,Ord a,Num a,Integral a,Show a,IArray UArray a) => EdgeWidget (BinEd a) where
    renderEdges = showbe

instance Bits a => WidgetKeyHandling (BinEd a)
instance (Num a,FiniteBits a,IArray UArray a) => KeyContextHandler (BinEd a) (BinEd a) where
    handleKeyCtx KRight      [] _ = nxcur
    handleKeyCtx KLeft       [] _ = pvcur
    handleKeyCtx KDown       [] _ = nxlin
    handleKeyCtx KUp         [] _ = pvlin
    handleKeyCtx (KChar '0') _  _ = modnxbe clearBit
    handleKeyCtx (KChar 'f') _  _ = modnxbe clearBit
    handleKeyCtx (KChar 'F') _  _ = modnxbe clearBit
    handleKeyCtx (KChar '1') _  _ = modnxbe setBit
    handleKeyCtx (KChar 't') _  _ = modnxbe setBit
    handleKeyCtx (KChar 'T') _  _ = modnxbe setBit
    handleKeyCtx (KChar 'c') _  _ = modnxbe complementBit
    handleKeyCtx (KChar 'C') _  _ = modnxbe complementBit
    handleKeyCtx (KChar '+') _  _ = modbe (\x _ -> x+1)
    handleKeyCtx (KChar '-') _  _ = modbe (\x _ -> x-1)
    handleKeyCtx (KChar '<') _  _ = modbe (\x _ -> rotateL x 1)
    handleKeyCtx (KChar '>') _  _ = modbe (\x _ -> rotateR x 1)
    handleKeyCtx (KChar 'h') _  _ = \x -> Just x { hex = not (hex x) }
    handleKeyCtx (KChar 'H') _  _ = \x -> Just x { hex = not (hex x) }
    handleKeyCtx _           _  _ = const Nothing

pvlin :: BinEd a -> Maybe (BinEd a)
pvlin be@BinEds{} = Just $ be { line_ = max 0 $ line_ be-1 }
pvlin _           = Nothing

rstcur :: (IArray UArray a,FiniteBits a) => BinEd a -> Maybe (BinEd a)
rstcur be@BinEd {} = Just $ be { cursor = finiteBitSize (value_ be)-1 }
rstcur be@BinEds {} = Just $ be { cursor = finiteBitSize (values_ be!1)-1 }

nxlin :: IArray UArray a => BinEd a -> Maybe (BinEd a)
nxlin be@BinEds{} | l0 < mx = Just $ be { line_ = l0+1}
    where mx = snd $ bounds $ values_ be
          l0 = line_ be
nxlin _           = Nothing

modnxbe :: (FiniteBits a,IArray UArray a) => (a -> Int -> a) -> BinEd a -> Maybe (BinEd a)
modnxbe f x | Nothing <- s2 = s1
            | otherwise = s2
    where s1 = modbe f x
          s2 = s1 >>= nxcur

modbe :: IArray UArray a => (a -> Int -> a) -> BinEd a -> Maybe (BinEd a)
modbe f be@BinEd{}  = Just $ be { value_ = f (value be) (cursor be) }
modbe f be@BinEds{} = Just $ be { values_ = vls//[(lns,f vl crs)] }
    where vls = values_ be
          lns = line be
          crs = cursor be
          vl = vls!lns

pvcur :: IArray UArray a => FiniteBits a => BinEd a -> Maybe (BinEd a)
pvcur be = Just $ be { cursor = min (finiteBitSize (value be)-1) (cursor be+1)}

nxcur :: (FiniteBits a,IArray UArray a) => BinEd a -> Maybe (BinEd a)
nxcur be@BinEds{} | cursor be == 0 = nxlin be >>= rstcur
nxcur be = Just $ be { cursor = max 0 (cursor be-1) }

showber :: (Integral a,Ord a,Num a,FiniteBits a,IArray UArray a) => Widget (BinEd a) -> DisplayRegion -> RenderContext -> IO Image
showber wg dr = fmap fst . showbe wg dr

showbe :: (Integral a,Ord a,Num a,FiniteBits a,IArray UArray a) => Widget (BinEd a) -> DisplayRegion -> RenderContext -> IO (Image,EdgeEmit)
showbe wg dr rc = do
    b <- getState wg
    return $ shbes nc fc dr (hex b) $ values b
    where nc = normalAttr rc
          fc = Attr (attrStyle nc) (SetTo (ISOColor 2)) (SetTo (ISOColor 0))

shbes :: (Integral a,Ord a,Num a,FiniteBits a) => Attr -> Attr -> DisplayRegion -> Bool -> [a] -> (Image,EdgeEmit)
shbes at au (w,h) hx xs = (im,EdgeEmit (imageWidth im) (imageHeight im) etb [] etb [])
    where bs = finiteBitSize $ head xs
          sh = hx && w-bs-1-div bs 4 >= 0
          im = foldr ((<->) . shbe at au sh w) emptyImage $ take h xs
          etb | sh = [bs+div (bs-1) 8]
              | otherwise = []

shbe :: (Integral a,Ord a,Num a,FiniteBits a) => Attr -> Attr -> Bool -> Int -> a -> Image
shbe at au sh mx vl = elc (min mx bs) <|> hxv
    where elm p | p < 0 = emptyImage
                | mod p 8 == 7 = cs <|> elc p
                | otherwise = elc p
          elc p | testBit vl p = c1 <|> tl
                | otherwise = c0 <|> tl
                where tl = elm $ p-1
          c0 = char at '0'
          c1 = char au '1'
          cs = char at ' '
          bs = finiteBitSize vl-1
          hxv | sh = char at '\x2502' <|> helm (bs-3)
              | otherwise = emptyImage
          helm p | p < 0 = emptyImage
                 | ni <= 9 = tl (48+ni)
                 | otherwise = tl (55+ni)
              where ni = shiftR vl p .&. 15
                    tl x = char at (chr $ fromIntegral x) <|> helm (p-4)

gbined :: (IArray UArray a,Show a,Integral a,Ord a,FiniteBits a) => BinEd a -> IO (Widget (BinEd a))
gbined st =
    newWidget st $ \w ->
        w {
            growHorizontal_ = const $ return False,
            growVertical_ = const $ return False,
            render_ = showber,
            keyEventHandler = handleKeyWidget,
            getCursorPosition_ = binGcp
        }

binGcp :: FiniteBits a => Widget (BinEd a) -> IO (Maybe (Int,Int))
binGcp wg = do
    x <- getState wg
    let cx = finiteBitSize (value_ x)-1-cursor x in shiftCursorWithPosition wg $ Just (cx+div cx 8,line x)

-- | Construct a binary editor that can modify a single bitwise value.
bined :: (IArray UArray a,Show a,Integral a,Ord a,FiniteBits a) => a -- ^ The original value to be stored, shown and altered by the binary editor.
    -> IO (Widget (BinEd a)) -- ^ The resulting I/O monad returning the constructed widget.
bined dat = gbined $ BinEd (finiteBitSize dat-1) dat True

-- | Construct a binary editor that can modify a series (list) of binary values.
bineds :: (Show a,Integral a,Ord a,FiniteBits a,IArray UArray a) => [a] -- The given list of original values to be shown and altered by the binary editor.
    -> IO (Widget (BinEd a)) -- ^ The resulting I/O monad returning the constructed widget.
bineds dat = gbined $ BinEds (finiteBitSize (head dat)-1) 0 (listArray (0,length dat-1) dat) True

scrollBineds dat = do
    w1 <- bineds dat
    autoScrollable w1

-- | Obtain the binary value currently hold by the data-structure. In case there are multiple displayed, the one on the cursor line is returned.
getBinValue :: (Bits a,IArray UArray a) => Widget (BinEd a) -- ^ The binary editor widget from which the value is obtained.
    -> IO a -- ^ The IO monad that returns the binary value currently stored by the binary editor.
getBinValue = getState >=> return . value

-- | Set the binary value stored in the widget and return the old value stored by the widget.
setBinValue :: (Bits a,IArray UArray a) => a -- ^ The new value to be stored and shown by the binary editor widget.
    -> Widget (BinEd a)  -- The widget to alter.
    -> IO a -- An I/O monad returning the old value stored in the widget.
setBinValue vl wg = do
    obe <- getState wg
    updateWidgetState wg $ \be -> be { value_ = vl }
    return $ value obe
