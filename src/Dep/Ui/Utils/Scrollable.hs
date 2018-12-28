{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dep.Ui.Utils.Scrollable (
    ScrollShow(..),
    ScrollConfig(..),
    genScrollable,scrollable,autoScrollable,autoScrollableH,autoScrollableV,alwaysScrollable,
    getScrollWidget,getScrollWidgetState,
    ScrollSt(..),
    PadScroll(..),
    padScrollable,
) where

import Control.Monad((>=>))

import Dep.Ui.Utils(vstring,KeyContextHandler(..),handleKeyWidget,WidgetKeyHandling(..),flld,propagateShiftSetCurrentPosition)
import Dep.Utils(border,selectNum)

import Graphics.Vty.Prelude(DisplayRegion)
import Graphics.Vty.Image(Image,string,imageWidth,imageHeight,pad,crop,translate,(<|>),(<->))
import Graphics.Vty.Input.Events(Key(..),Modifier(..))
import Graphics.Vty.Widgets.All(Widget,newWidget,WidgetImpl(..),RenderContext(..),getState)
import Graphics.Vty.Widgets.Core(handleKeyEvent,render,growHorizontal,growVertical,relayFocusEvents,updateWidgetState,getCursorPosition)

-- | A typeclass that specifies that a widget can be rendered only partly, making it more efficiently to render it with scrollbars.
class PadScroll a where
    -- | A render function, but where only a specific part of the image is rendered. Used to optimize scrollbar rendering.
    renderPad :: DisplayRegion -- The top left coordinate at which the image should begin
        -> Widget a -- The widget that should be rendered.
        -> DisplayRegion -- The width and the height of the image to render
        -> RenderContext -- The accompanied rendercontext that contains the attributes and style in general.
        -> IO (Image,DisplayRegion) -- An I/O monad returning the image as well as the size of the entire (virtual) image.

-- | The default way to render with a render pad is call the internal render function with an (almost) boundless display region and perform the padding oneself.
defaultRenderPad :: Show a => DisplayRegion -- ^ The offset of the padding.
    -> Widget a -- ^ The widget that is to be rendered with padding.
    -> DisplayRegion -- ^ The  width and the height restrictions of the scrollbar.
    -> RenderContext -- ^ The render context containing the style of the rendering.
    -> IO (Image,DisplayRegion) -- ^ The I/O monad returning both the image and the dimensions of the full imagee.
defaultRenderPad (sx,sy) wdg (w,h) rc = do
    im <- render wdg (mx,mx) rc
    let {iw = imageWidth im; ih = imageHeight im} in return (pad 0 0 (max 0 $ w-iw) (max 0 $ h-ih) $ crop w h $ translate (-sx) (-sy) im,(iw,ih))
    where mx = maxBound

-- | An enumeration that specifies three possible ways to use a specific scrollbar.
data ScrollShow = ScrollNever -- ^ The scrollbar is never shown, but the widget reacts on keystrokes to reposition the underlying widget.
     | ScrollAuto -- ^ The scrollbar is shown if the underlying widget is greater than what is allowed to be rendered.
     | ScrollAlways -- ^ The scrollbar is always shown, regardless whether the underlying widget is larger or not.
     deriving (Show,Eq,Enum,Bounded,Ord)

-- | The scrollbar configuration. Contains a `ScrollShow` element for both directions.
data ScrollConfig = ScrollConfig { 
        horizontalScroll :: ScrollShow, -- ^ The configuration of the horizontal scrollbar.
       verticalScroll :: ScrollShow -- ^ The configuration of the vertical scrollbar.
    } deriving (Show,Eq)

-- | The state of the scrollbar, contains the way both scollbars are configured as well as the current cursor position and the internal widget to represent.
data ScrollSt a = Show a => ScrollSt {
          config :: ScrollConfig -- ^ The configuration of the scrollbar; describing when a scrollbar should be shown.
        , sxy :: (Int,Int) -- ^ The coordinate where the padding happens of the internal widget.
        , wh :: (Int,Int) -- ^ The width and the height of the internal widget; updated when rendered.
        , widget :: Widget a -- ^ The internal widget that is made scrollable.
        , padRenderer :: DisplayRegion -> Widget a -> DisplayRegion -> RenderContext -> IO (Image,DisplayRegion) -- ^ The padded renderer function for the widget.
    }

-- | Obtain the internal widget (for instance to alter the widget).
getScrollWidget :: Widget (ScrollSt a) -- ^ The scrollbar widget.
     -> IO (Widget a) -- ^ An I/O monad returining the widget contained by the scrollbar.
getScrollWidget = getState >=> return . widget

-- | Get the state of the internal widget, for instance to perform calculations.
getScrollWidgetState :: Widget (ScrollSt a)
     -> IO a -- An I/O monad returning the state of the widget contained by the scrollable widget.
getScrollWidgetState = getState >=> getState . widget

-- | A scrollbar reacts on the Ctrl+numpad arrows to position the cursor to the left, right,...
instance KeyContextHandler (ScrollSt a) b where
    handleKeyCtx KBS         _ _ = moveSc (0,-10)
    handleKeyCtx (KChar c) m _ | elem MCtrl m = f c
        where f '4' = moveSc (-10, 0)
              f '$' = moveSc (-10, 0)
              f '6' = moveSc (10, 0)
              f '^' = moveSc (10, 0)
              f '8' = moveSc (0, -10)
              f '*' = moveSc (0, -10)
              f '2' = moveSc (0, 10)
              f '@' = moveSc (0, 10)
              f _ = const Nothing
    handleKeyCtx _           _ _ = const Nothing

moveSc :: (Int,Int) -> ScrollSt a -> Maybe (ScrollSt a)
moveSc (dx,dy) s@ScrollSt{sxy=xy0@(x0,y0), wh=(w,h)}
    | xy0 /= nxy = Just (s {sxy=nxy})
    | otherwise = Nothing
    where nxy = (border 0 (w-1) (dx+x0), border 0 (h-1) (dy+y0))

-- | A scroll state handles the keystroke first. In case that does not resolve the issue, it passes the even to its child.
instance WidgetKeyHandling (ScrollSt a) where
    handleKeyWidgetFallback w k m = getState w >>= \x -> handleKeyEvent (widget x) k m

-- | A scroll state can show relevant information about its state.
instance Show (ScrollSt a) where
    show x = "[ScrollSt"++show (sxy x)++"]"

scrollString :: a -> a -> a -> a -> Int -> Int -> Int -> [a] --TODO: fix length string
scrollString ce ca cm cb wo wi idx = take wo $ replicate si ce++ca : replicate (sj-2) cm++ cb : replicate (wo-si-sj) ce
    where sj = max 0 $ min wo $ div (wo*wo) wi
          si = max 0 $ min (wo-sj) $ div ((wo-sj)*idx) (wi-wo)

genScrollable :: Show a => (DisplayRegion -> Widget a -> DisplayRegion -> RenderContext -> IO (Image,DisplayRegion)) -> ScrollConfig -> Widget a -> IO (Widget (ScrollSt a))
genScrollable f s w = do
        wo <- newWidget (ScrollSt s (0,0) (0,0) w f) $ \x -> x {
            growHorizontal_ = growHorizontal . widget,
            growVertical_ = growVertical . widget,
            render_ = innerRender,
            getCursorPosition_ = getState >=> adjustCursorPosition,
            keyEventHandler = handleKeyWidget,
            setCurrentPosition_ = propagateShiftSetCurrentPosition (\ScrollSt {sxy=(sx,sy)} -> return (-sx,-sy)) (return . widget)
        }
        relayFocusEvents wo w
        return wo

-- | Create a new scrollable widget with the given configuration for the given widget.
scrollable :: Show a => ScrollConfig --  ^ The given configuration on how the scrollbar should behave.
    -> Widget a -- ^ The widget that is shown (partly) by the scrollable.
    -> IO (Widget (ScrollSt a)) -- ^ A widget that shows a part of the inner widget such that the screen dimensions are not overflown.
scrollable = genScrollable defaultRenderPad

-- | Create a new scrollable widget with a given configuration and a widget that supports padded rendering.
padScrollable :: (Show a,PadScroll a) => ScrollConfig
    -> Widget a -- ^ The widget that is shown (partly) by the scrollable.
    -> IO (Widget (ScrollSt a))
padScrollable = genScrollable renderPad

adjustCursorPosition :: ScrollSt a -> IO (Maybe (Int,Int))
adjustCursorPosition ss = do
    c <- getCursorPosition $ widget ss
    return $ case c of
        Just ce@(_,_) -> addCur ss ce
        _ -> c

addCur :: ScrollSt a -> (Int,Int) -> Maybe (Int,Int)
addCur ss (cx,cy) | tx >= 0 && ty >= 0 = Just (tx,ty)
                 | otherwise = Nothing
    where (sx,sy) = sxy ss
          tx = cx-sx
          ty = cy-sy -- TODO: additional bound checks

autoScrollable :: Show a => Widget a -> IO (Widget (ScrollSt a))
autoScrollable = scrollable $ ScrollConfig ScrollAuto ScrollAuto

autoScrollableH :: Show a => Widget a -> IO (Widget (ScrollSt a))
autoScrollableH = scrollable $ ScrollConfig ScrollNever ScrollAuto

autoScrollableV :: Show a => Widget a -> IO (Widget (ScrollSt a))
autoScrollableV = scrollable $ ScrollConfig ScrollAuto ScrollNever

alwaysScrollable :: Show a => Widget a -> IO (Widget (ScrollSt a))
alwaysScrollable = scrollable $ ScrollConfig ScrollAlways ScrollAlways

shsc :: ScrollShow -> Int -> Int -> Int
shsc ScrollNever _ _ = 0
shsc ScrollAuto  x y | x < y = 1
                     | otherwise = 0
shsc _           _ _ = 1

scrd :: Show a => Widget (ScrollSt a) -> DisplayRegion -> RenderContext -> ScrollSt a -> Image -> DisplayRegion -> IO Image
scrd ws (dw,dh) c s inr (iw,ih) = do
    updateWidgetState ws $ const s {wh=(max 0 (iw-dw1),max 0 (ih-dh1))}
    return $ selectNum shv av id $ selectNum shh ah id $ crop dw1 dh1 inr
    where na = normalAttr c
          cfg = config s
          shh = shsc (horizontalScroll cfg) dw iw
          shv = shsc (verticalScroll cfg) dh ih
          dw1 | iw < dw = iw
              | otherwise = dw-shv
          dh1 | ih < dh = ih
              | otherwise = dh-shh
          (sxs,sys) = sxy s
          f1 = flld 1
          f3 = flld 3
          scs = scrollString f1 f3 f3 f3
          av x = x <|> vstring na (scs dh1 ih sys)
          ah x = x <-> string na (scs dw1 iw sxs)

innerRender :: Show a => Widget (ScrollSt a) -> DisplayRegion -> RenderContext -> IO Image
innerRender w dm c = do
    s <- getState w
    (inr,dwh) <- padRenderer s (sxy s) (widget s) dm c
    scrd w dm c s inr dwh
