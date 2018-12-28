{-# LANGUAGE RankNTypes #-}

module Dep.Ui.Utils.Boxes (
        Boxes(),hBoxes,vBoxes,setBoxesSpacing,
        getBoxChildSizePolicy,setBoxChildSizePolicy,
        withBoxesSpacing,switchOrientation,Orientation(..)
    ) where

import Control.Exception(throw)
import Control.Monad(liftM,filterM)

import Data.List(intersperse)

import Graphics.Vty.Attributes(Attr(..))
import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,imageWidth,imageHeight,emptyImage,charFill,(<|>),(<->))
import Graphics.Vty.Input.Events(Key,Modifier)
import Graphics.Vty.Widgets.All(Widget,newWidget,WidgetImpl(..),RenderContext(..),getNormalAttr,getState)
import Graphics.Vty.Widgets.Core(handleKeyEvent,render,getCursorPosition,getCurrentSize,setCurrentPosition,growHorizontal,growVertical,relayFocusEvents,updateWidgetState,(<~~),FocusGroup(..),newFocusGroup,addToFocusGroup)
import Graphics.Vty.Widgets.Box(IndividualPolicy(..),BoxError(..))
import Graphics.Vty.Widgets.Util(plusWidth,plusHeight,withWidth,withHeight)

import Dep.Utils(setFst,setSnd,distribution100)
import Dep.Utils.MdUtils(orBoolM,orMaybeM)
import Dep.Ui.Utils(isFocused)

data Orientation = Horizontal | Vertical deriving (Eq,Enum,Bounded,Show)

data ChildsSizePolicy = PerChild [IndividualPolicy] | Percentage [Int] deriving (Show, Eq)

data Boxes a = Boxes { boxChildsSizePolicy :: ChildsSizePolicy, boxItems :: [Widget a], boxOrientation :: Orientation, boxSpacing :: Int, grows :: [IO Bool],  imgCat :: Image -> Image -> Image, regDimension :: DisplayRegion -> Int, imgDimension :: Image -> Int, withDimension :: DisplayRegion -> Int -> DisplayRegion }

opposite :: Orientation -> Orientation
opposite Horizontal = Vertical
opposite Vertical = Horizontal

orDim :: Orientation -> Image -> Int
orDim Horizontal = imageWidth
orDim _          = imageHeight

orWith :: Orientation -> DisplayRegion -> Int -> DisplayRegion
orWith Horizontal = withWidth
orWith _          = withHeight

orDim2 :: Orientation -> Image -> Int
orDim2 Horizontal = imageHeight
orDim2 _          = imageWidth

orGet :: Orientation -> (a,a) -> a
orGet Horizontal = fst
orGet _          = snd

orSet2 :: Orientation -> a -> (a,a) -> (a,a)
orSet2 Horizontal = setSnd
orSet2 _          = setFst

orImgCat :: Orientation -> Image -> Image -> Image
orImgCat Horizontal = (<|>)
orImgCat _          = (<->)

orGrow :: Orientation -> Widget a -> IO Bool
orGrow Horizontal = growHorizontal
orGrow _          = growVertical

spacer :: Attr -> Int -> Orientation -> [Image] -> Image
spacer _ 0 _ _  = emptyImage
spacer a n o is = charFillTpl a ' ' dm
    where s = maximum $ map (orDim2 o) is
          dm = orSet2 o s (n,n)

charFillTpl :: Attr -> Char -> (Int,Int) -> Image
charFillTpl a c = uncurry (charFill a c)

instance Show (Boxes a) where
    show b = concat ["Box { spacing = ", show $ boxSpacing b,
                     ", childSizePolicy = ", show $ boxChildsSizePolicy b,
                     ", orientation = ", show $ boxOrientation b,
                     " }"]

hBoxes :: Show a => [Widget a] -> IO (Widget (Boxes a),Widget FocusGroup)
hBoxes = boxes Horizontal 0

vBoxes :: Show a => [Widget a] -> IO (Widget (Boxes a),Widget FocusGroup)
vBoxes = boxes Vertical 0

defaultChildsSizePolicy :: Int -> ChildsSizePolicy
defaultChildsSizePolicy n = PerChild $ replicate n BoxAuto

boxes :: Show a => Orientation -> Int -> [Widget a] -> IO (Widget (Boxes a),Widget FocusGroup)
boxes o sp ws = do
    let initSt = Boxes {
        boxChildsSizePolicy = defaultChildsSizePolicy $ length ws,
        boxOrientation = o,
        boxSpacing = sp,
        boxItems = ws,
        grows = map (orGrow o) ws,
        imgCat = orImgCat o,
        regDimension = orGet o,
        imgDimension = orDim o,
        withDimension = orWith o
    }
    fg <- newFocusGroup
    mapM_ (addToFocusGroup fg) ws
    wRef <- newWidget initSt $ \w -> --WARNING: make sure no `o` is given in the function: orientation can change at runtime.
        w {
            growHorizontal_ = growPolicy growHorizontal Horizontal,
            growVertical_ = growPolicy growVertical Vertical,
            keyEventHandler = \this key mods -> getState this >>= orBoolM (\x -> handleKeyEvent x key mods) . boxItems,
            render_ = \this s ctx -> getState this >>= renderBoxes s ctx,
            getCursorPosition_ = gcp,
            setCurrentPosition_ = \this dr -> getState this >>= flip setCurrentPos dr
        }
    mapM_ (relayFocusEvents wRef) ws
    return (wRef,fg)

gcp :: Widget (Boxes a) -> IO (Maybe (Int,Int))
gcp wg = do
    st <- getState wg
    fcd <- filterM isFocused $ boxItems st
    orMaybeM getCursorPosition fcd

switchOrientation :: Widget (Boxes a) -> IO ()
switchOrientation w = updateWidgetState w $ \b -> b { boxOrientation = opposite $ boxOrientation b }

setBoxesSpacing :: Widget (Boxes a) -> Int -> IO ()
setBoxesSpacing wRef spacing = updateWidgetState wRef $ \b -> b { boxSpacing = spacing }

withBoxesSpacing :: Int -> Widget (Boxes a) -> IO (Widget (Boxes a))
withBoxesSpacing spacing wRef = do
    setBoxesSpacing wRef spacing
    return wRef

getBoxChildSizePolicy :: Widget (Boxes a) -> IO ChildsSizePolicy
getBoxChildSizePolicy = (boxChildsSizePolicy <~~)

setBoxChildSizePolicy :: Widget (Boxes a) -> ChildsSizePolicy -> IO ()
setBoxChildSizePolicy b p@(Percentage l) | distribution100 l = sbcp b l p
setBoxChildSizePolicy b p@(PerChild l)   = sbcp b l p
setBoxChildSizePolicy _ _                = throw BadPercentage

sbcp :: forall a b . Widget (Boxes b) -> [a] -> ChildsSizePolicy -> IO ()
sbcp w i p = do
    s <- getState w
    if length i == length (boxItems s) then updateWidgetState w $ \b -> b {boxChildsSizePolicy = p} else throw BadPercentage

setCurrentPos :: Boxes a -> DisplayRegion -> IO ()
setCurrentPos b | boxOrientation b == Horizontal = scp fst plusWidth (boxSpacing b) (boxItems b)
                | otherwise = scp snd plusHeight (boxSpacing b) (boxItems b)
    where scp :: (DisplayRegion -> Int) -> (DisplayRegion -> Int -> DisplayRegion) -> Int -> [Widget a] -> DisplayRegion -> IO ()
          scp fa fb s = scpi
              where scpi :: [Widget a] -> DisplayRegion -> IO ()
                    scpi (x:xs) pos = do
                        szi <- getCurrentSize x
                        setCurrentPosition x pos
                        scpi xs $ fb pos (fa szi + s)
                    scpi _ _ = return ()

growPolicy :: (Widget a -> IO Bool) -> Orientation -> Boxes a -> IO Bool
growPolicy g h b | h == boxOrientation b = fmap or boxbmap
                 | PerChild ips <- boxChildsSizePolicy b = fmap (or . zipWith (\x y -> y && x == BoxAuto) ips) boxbmap
                 | otherwise = return False --True??
                 where boxbmap = mapM g $ boxItems b

boxesKeyEventHandler :: Widget (Boxes a) -> Key -> [Modifier] -> IO Bool
boxesKeyEventHandler this key mods = do
    b <- getState this
    f $ boxItems b
        where f (x:xs) = do
                  handled <- handleKeyEvent x key mods
                  if handled then return True else f xs
              f _      = return False

renderBoxes :: Show a => DisplayRegion -> RenderContext -> Boxes a -> IO Image
renderBoxes s ctx this = do
    is <- mapM (\x -> render x s ctx) $ boxItems this
    return $ foldl1 (imgCat this) $ intersperse (spacer a (boxSpacing this) Horizontal is) is
    where a = normalAttr ctx

rBoxes :: Show a => DisplayRegion -> RenderContext -> Boxes a -> IO Image
rBoxes d r b = rBoxes' (boxChildsSizePolicy b)
    where rBoxes' :: ChildsSizePolicy -> IO Image
          rBoxes' (Percentage ls) = throw BadPercentage
          rBoxes' (PerChild ls) = throw BadPercentage
