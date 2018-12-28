{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | A module that contains utility functions to render and built `Widget`s in a more convenient way.
module Dep.Ui.Utils (
    isFocused,getCurrentSize,getCurrentWidth,getCurrentHeight,          -- Read properties
    shiftCursorWithPosition,                                            -- Utility functions for widgets cursor
    propagateSetCurrentPosition,propagateShiftSetCurrentPosition,       -- Utility functions for widgets position
    picaH,picaW,flld,linC,                                              -- Special character mappings
    linu,linU,linr,linR,lind,linD,linl,linL,                            -- Box drawing specifiers.
    genReplicate,replicateW,replicateH,vstring,                         -- Replicate characters in directions
    drawFF,drawDff,drawTff,drawSRff,drawJKff,drawFFinpW,                -- Draw flipflops
    EdgeEmit(..),edgeEmitW,edgeEmitH,EdgeWidget(..),zeroEmit,branchW,branchH,branchWinit,branchHinit,defaultRenderEdges,RenderEmitter,
    swapAttr,
    KeyContextHandler(..),Decorator(..),UiDecorator(..),WidgetKeyHandling(..),
    imageReplicate,alternate,identity,matrixImg,
    genericFlatImage,flatImg,composeImg,flattenImg,tileImg,
    handleKeyWidget,
    uiCurX,uiCurY,uiOpt,
    tapline,taplines,inboxH,lineLabel,vlineILabel,
    mapImg,mapHImg,
    calcRout,routImg,calcRoutImg,
    wireAttr,highlightAttr,highlightWireAttr
    ) where

import Control.Arrow(first,second)

import qualified Data.Array.Unboxed as UA
import Data.Bits(shiftL,(.&.),(.|.))
import Data.BitVector(BV(),bitVec,zeros)
import Data.Function(const,(.))
import Data.Hashable(Hashable(hashWithSalt))
import Data.IORef(readIORef)
import Data.List(map)
import Data.Word(Word8)

import Dep.Structures(BitTh(..))
import Dep.Utils(border,replicateFoldl1,fTup,overMN,constN,fastSortByOn,selN,mergeOrdOn,mapWithf,sortOn,mapN,selectBool,powerl)

import Graphics.Vty.Attributes(Attr(..),Color(ISOColor),MaybeDefault(SetTo))
import Graphics.Vty.Image(DisplayRegion(),Image(),string,char,imageWidth,imageHeight,crop,vertCat,emptyImage,(<|>),(<->))
import Graphics.Vty.Input.Events(Key(..),Modifier(..))
import Graphics.Vty.Widgets.All(Widget(),WidgetImpl(..),getState,RenderContext(..),render,setCurrentPosition,getCurrentPosition)
import Graphics.Vty.Widgets.Core(updateWidgetState,(<~))

-- | Generate an attribute based on the data the wire contains or works on.
wireAttr :: Hashable a => Attr -- ^ The given normal attribute.
    -> a -- The given data the wire transports.
    -> Attr -- ^ The resulting wire attribute.
wireAttr (Attr aa _ ac) x = Attr aa (SetTo $ ISOColor $ shiftL 1 $ mod (hashWithSalt 365 x) 3) ac

-- | Convert the given attribute into its highlighted equivalent if the given condition is met.
highlightAttr :: Bool -- ^ The given condition to check for.
    -> Attr -- ^ The given attribute that should be converted into its highlighted equivalent.
    -> Attr -- ^ The resulting attribute that is the highlighted equivalent if the given condition is met. Otherwise the given attribute.
highlightAttr False x = x
highlightAttr True (Attr _ (SetTo (ISOColor x)) ac) = Attr (SetTo 32) (SetTo $ ISOColor $ shl x) ac
    where shl 1 = 3
          shl 2 = 6
          shl 4 = 5
          shl y = y
highlightAttr True x = x

-- | Generate an attribute based on the data the wire contains or works on and optionally highlight it.
highlightWireAttr :: Hashable a => Attr
    -> Bool -- ^ The given boolean indicating whether the wire should be highlighted.
    -> a -- ^ The given data the wire transports.
    -> Attr -- ^ The resulting wire attribute.
highlightWireAttr n b = highlightAttr b . wireAttr n


-- | Determine whether the given `Widget` currently has focus.
isFocused :: Widget a -- ^ The given Widget to determine whether it has focus.
    -> IO Bool -- ^ The resulting boolean that is True if the given Widget has focus, False otherwise.
isFocused = (<~) focused

-- | Get the current size of the given Widget as a tuple containing the width and height.
getCurrentSize :: Widget a -- ^ The given Widget to determine its current size.
    -> IO DisplayRegion -- ^ The resulting tuple that contains the width and the height of the Widget.
getCurrentSize = (<~) currentSize

-- | Get the current width of the given Widget as the amount of columns.
getCurrentWidth :: Widget a -- ^ The given Widget to determine its current width.
    -> IO Int -- ^ The resulting width of the given Widget in columns.
getCurrentWidth = (<~)  $ fst . currentSize

-- | Get the current height of the given Widget as the amount of rows.
getCurrentHeight :: Widget a -- ^ The given Widget to determine its current height.
    -> IO Int -- ^ The resulting height of the given Widget in rows.
getCurrentHeight = (<~) $ snd . currentSize

-- | Shift the given cursor position by the given position. Since most cursor positions are relative to their `Widget` location, this is a popular utility function.
shiftCursorWithPosition :: Widget a -- ^ The given `Widget` to determine the position from.
    -> Maybe DisplayRegion -- ^ The given relative position. Can be `Nothing` if no cursor should be displayed.
    -> IO (Maybe DisplayRegion) -- ^ An I/O monad that returns the shifted cursor position.
shiftCursorWithPosition _ Nothing = return Nothing
shiftCursorWithPosition w (Just (cx,cy)) = do
    (px,py) <- getCurrentPosition w
    return $ Just (cx+px,cy+py)

-- | Propagate the current position unchanged further to a child that is generated by a given function.
propagateSetCurrentPosition :: (a -> IO (Widget b)) -- ^ The given generator that generates for a given `Widget` state the child it should propagate to.
    -> Widget a -- ^ The given `Widget` that received the current position and should propagate it.
    -> DisplayRegion -- ^ The given `DisplayRegion`: the position of the given `Widget`.
    -> IO () -- ^ Returns nothing, this is a pure I/O function.
propagateSetCurrentPosition chg wg dr = do
    st <- getState wg
    ch <- chg st
    setCurrentPosition ch dr

-- | Propagate the current position with a generated shift to its generated child.
propagateShiftSetCurrentPosition :: (a -> IO DisplayRegion) -- ^ The given generator that generates for a given `Widget` state the shift vector that should be used.
    -> (a -> IO (Widget b)) -- ^ The given generator that generates for a given `Widget` state the child it should propagate.
    -> Widget a -- ^ The given `Widget` that received the current position and should propagate it.
    -> DisplayRegion -- ^ The given `DisplayRegion`: the position of the given `Widget`.
    -> IO () -- ^ Returns nothing, this is a pure I/O function.
propagateShiftSetCurrentPosition gn chg wg (cx,cy) = do
    st <- getState wg
    (sx,sy) <- gn st
    ch  <- chg st
    setCurrentPosition ch (cx+sx,cy+sy)

-- | A function to draw an arbitrary flipflop with the two (or one and a space) characters determining the input side.
drawFF :: Char -- ^ The given character to be placed at the top of the flipflop's input side.
    -> Char -- ^ The given character to be placed at the bottom of the flipflop's output side.
    -> Attr -- ^ The given attribute to render the flipflop.
    -> Image -- ^ The resulting flipflop image.
drawFF ct cb atr = tp0 <-> tp1 <-> mid <-> bt1 <-> bt0
    where sa = string atr
          hl = replicate 3 $ linC 136
          vl = linC 34
          vli = linC 98
          vls = [vl]
          vlo = [linC 38]
          tp0 = sa $ linC 40 : hl ++ [linC 160]
          tp1 = sa $ vli : ct : ' ' : 'Q' : vlo
          bt1 = sa $ selectBool (cb /= ' ') vli vl : cb : ' ' : 'Q' : vlo
          mid = sa $ vli : '>':' ':'_':vls
          bt0 = sa $ linC 10 : hl ++ [linC 130]

-- | Image of a data flipflop
drawDff :: Attr -- ^ The given attribute to render the flipflop.
    -> Image -- ^ The resulting flipflop image.
drawDff = drawFF 'D' ' '

-- | Image of a toggle flipflop
drawTff :: Attr -- ^ The given attribute to render the flipflop.
    -> Image -- ^ The resulting flipflop image.
drawTff = drawFF 'T' ' '

-- | Image of a set-reset flipflop
drawSRff :: Attr -- ^ The given attribute to render the flipflop.
    -> Image -- ^ The resulting flipflop image.
drawSRff = drawFF 'S' 'R'

-- | Image of a Jack Kilby flipflop
drawJKff :: Attr -- ^ The given attribute to render the flipflop.
    -> Image -- ^ The resulting flipflop image.
drawJKff = drawFF 'J' 'K'

-- | Draw wires at the input side of the flip-flop such that the bottom and top connections are moved upwards and the clock entry is moved downwards.
drawFFinpW :: Bool -- ^ The given boolean that determines whether this is a flipflop with two data inputs (True), or one input entry (False).
    -> Attr -- ^ The given attribute to render whitespace.
    -> Attr -- ^ The given attribute to render the top wire (D,T,S or J)
    -> Attr -- ^ The given attribute to render the bottom wire (R or K)
    -> Attr -- ^ The given attribute to render the the clock wire.
    -> Image -- ^ An image containing the wires that connect the flipflop together.
drawFFinpW cnd cn ca cb cc | cnd = luf <-> l0f <-> l1f <-> l2f <-> l3f
                           | otherwise = lut <-> l0t <-> l1t <-> l2t <-> l3t
    where c3 = linC 5
          c5 = linC 17
          c6 = linC 20
          luf = char ca c5
          l0f = char ca c3
          l1f = char cb c6
          l2f = char cb c5
          l3f = char cb c5
          lut = char cc c5 <|> char ca c5
          l0t = char cc c5 <|> char ca c3
          l1t = char cc c5 <|> char cb c6
          l2t = char cc c3 <|> char cb c5
          l3t = char cn ' ' <|> char cb c5

data WreMp = WreMp { wreW :: Int, wreH :: Int, wreD :: UA.UArray Int Word8 }

instance Show WreMp where
    show (WreMp w h d) = shi 0
        where n = w*h
              shi i | i >= n = ""
                    | i <= 0 = tl
                    | otherwise = '\n' : tl
                   where iw = i+w
                         ln = concatMap (show . (UA.!) d) [i..iw-1]
                         tl = ln ++ shi iw

hops :: WreMp -> (Int,Int) -> [(Int,(Int,Int))]
hops wm fs = concat [mvdr fs (1,0) 1 1,mvdr fs (-1,0) 1 1,mvdr fs (0,1) 2 1]
    where ww = wreW wm
          hh = wreH wm
          aa = wreD wm
          mvdr (x,y) d@(dx,dy) cr cc | nx < 0 || nx >= ww || ny < 0 || ny >= hh = []
                                     | aann == cr = mvdr n d cr (cc+1)
                                     | aann /= 0 = []
                                     | otherwise = [(cc,(x+dx,y+dy))]
              where n@(nx,ny)=(x+dx,y+dy)
                    nn = ww*ny+nx
                    aann = (UA.!) aa nn

zeroWireMap :: Int -> Int -> WreMp
zeroWireMap w h = WreMp w h $ UA.array (0,w*h-1) $ map (,0) [0..w*h-1]

-- | A generic function that takes as input a merger function, the number of times the character should be replicated, the given attribute to render the characters and the given character. The function produces an image by calling the merger between all replicas of characters with the given attribute.
genReplicate :: (Image -> Image -> Image) -- ^ The given image merger function.
    -> Int -- ^ The given number of times the character should be replicated.
    -> Attr -- ^ The given attribute that determines how the character should be rendered.
    -> Char -- ^ The given character to be rendered.
    -> Image -- ^ The resulting image produces by replicating the given character with the given attribute and calling the merger function in between.
genReplicate mgr n at c = powerl emptyImage (char at c) mgr n

-- | A function that takes as input the given number of repetitions, the attribute that determines how to render the characters and a character and produces an image of `1xw` of the given character.
replicateW :: Int -- ^ The given number of repetitions (and thus columns).
    -> Attr -- ^ The given attribute that determines how to render the character.
    -> Char -- ^ The given character to be repeatedly rendered.
    -> Image -- ^ The resulting image that consists out of one row and the given number of columns.
replicateW = genReplicate (<|>)

-- | A function that takes as input the given number of repetitions, the attribute that determines how to render the characters and a character and produces an image of `hx1` of the given character.
replicateH :: Int -- ^ The given number of repetitions (and thus rows).
    -> Attr -- ^ The given attribute that determines how to render the character.
    -> Char -- ^ The given character to be repeatedly rendered.
    -> Image -- ^ The resulting image that consists out of one column and the given number of rows.
replicateH = genReplicate (<->)

genBranch :: Char -> Char -> Char -> Char -> (Image -> Image -> Image) -> Int -> Attr -> Int -> [Int] -> [Int] -> Image
genBranch c0 cl cr cc mgr i0 at wd = bh i0
    where bh i (a:as) bs | a < i = bh i as bs
          bh i as (b:bs) | b < i = bh i as bs
          bh i al@(a:as) bl@(b:bs) | wd < mab = pre (wd-i)
                                   | a == b = tl cc as bs
                                   | a < b = tl cl as bl
                                   | otherwise = tl cr al bs
              where mab = min a b
                    mabw = min mab wd
                    premi = pre (mabw-i)
                    tl im x y = mgr premi $ mgr (char at im) $ bh (mab+1) x y
          bh i (a:as) [] | wd < a = pre (wd-i)
                         | otherwise = prel i a cl as []
          bh i [] (b:bs) | wd < b = pre (wd-i)
                         | otherwise = prel i b cr [] bs
          bh i [] [] = pre (wd-i)
          pre d = genReplicate mgr d at c0
          prel i d ca l r = mgr (pre (d-i)) $ mgr (char at ca) $ bh (d+1) l r

branchH :: Attr -> Int -> [Int] -> [Int] -> Image
branchH = branchHinit 0

branchHinit :: Int -> Attr -> Int -> [Int] -> [Int] -> Image
branchHinit = genBranch (linC 17) (linC 81) (linC 21) (linC 85) (<->)

branchW :: Attr -> Int -> [Int] -> [Int] -> Image
branchW = branchWinit 0

branchWinit :: Int -> Attr -> Int -> [Int] -> [Int] -> Image
branchWinit = genBranch (linC 68) (linC 69) (linC 84) (linC 85) (<|>)

data Decorator a b = Decorator { decorstate :: a, decorators :: [b] } deriving Show
data UiDecorator = CursorX { minX :: Int, maxX :: Int, cursorX :: Int } | CursorY { minY :: Int, maxY :: Int, cursorY :: Int } | Option { toggle :: Bool, key :: Key, aim :: Int} | Spinner { spin :: Int, spinMax :: Int, key :: Key, aim :: Int } deriving Show
data EdgeEmit = EdgeEmit { emitWidth :: Int, emitHeight :: Int, emitTop :: [Int], emitRight :: [Int], emitBottom :: [Int], emitLeft :: [Int] }

zeroEmit :: EdgeEmit
zeroEmit = EdgeEmit 0 0 [] [] [] []

type RenderEmitter a = Widget a -> DisplayRegion -> RenderContext -> IO (Image,EdgeEmit)

defaultRenderEdges :: Show a => RenderEmitter a
defaultRenderEdges w d r = do
    img <- render w d r
    return (img,EdgeEmit (imageWidth img) (imageHeight img) [] [] [] [])

class EdgeWidget a where
    renderEdges :: Show a => RenderEmitter a
    renderEdges = defaultRenderEdges

edgeEmitW :: Int -> EdgeEmit -> EdgeEmit -> EdgeEmit
edgeEmitW dw (EdgeEmit wa ha ta _ ba la) (EdgeEmit wb hb tb rb bb _) = EdgeEmit (dww+wb) (max ha hb) (cnc ta tb) rb (cnc ba bb) la
    where dww = dw+wa
          cnc x y = x++map (dww+) y

edgeEmitH :: Int -> EdgeEmit -> EdgeEmit -> EdgeEmit
edgeEmitH dh (EdgeEmit wa ha ta ra _ la) (EdgeEmit wb hb _ rb bb lb) = EdgeEmit (max wa wb) (dhh+hb) ta (cnc ra rb) bb (cnc la lb)
    where dhh = dh+ha
          cnc x y = x++map (dhh+) y

type WireC = (Int,Attr)
type WireR = ([Int],Attr)

picaH :: Int -> Int -> Char
picaH 1 1 = '\x2514'
picaH 1 2 = '\x250c'
picaH 1 3 = '\x251c'
picaH 2 1 = '\x2518'
picaH 2 2 = '\x2510'
picaH 2 3 = '\x2524'
picaH _ 1 = '\x2534'
picaH _ 2 = '\x252c'
picaH _ 3 = '\x253c'
picaH _ _ = ' '

picaW :: Int -> Int -> Char
picaW 1 1 = '\x2514'
picaW 1 2 = '\x250c'
picaW 1 3 = '\x251c'
picaW 2 1 = '\x2518'
picaW 2 2 = '\x2510'
picaW 2 3 = '\x2524'
picaW _ 1 = '\x2534'
picaW _ 2 = '\x252c'
picaW _ 3 = '\x253c'
picaW _ _ = ' '

-- | Specify that the char contains a line to the up that is not bold. Can be used for convenience when using `linC`.
linu :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linu = 1

-- | Specify that the char contains a line to the up that is bold. Can be used for convenience when using `linC`.
linU :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linU = 2

-- | Specify that the char contains a line to the right that is not bold. Can be used for convenience when using `linC`.
linr :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linr = 4

-- | Specify that the char contains a line to the right that is bold. Can be used for convenience when using `linC`.
linR :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linR = 8

-- | Specify that the char contains a line to the down that is not bold. Can be used for convenience when using `linC`.
lind :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
lind = 16

-- | Specify that the char contains a line to the down that is bold. Can be used for convenience when using `linC`.
linD :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linD = 32

-- | Specify that the char contains a line to the left that is not bold. Can be used for convenience when using `linC`.
linl :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linl = 64

-- | Specify that the char contains a line to the left that is bold. Can be used for convenience when using `linC`.
linL :: Int -- ^ The resulting integer specifying that the char should contain this box drawing line.
linL = 128

-- | A semantical mapping of lines to the corresponding box-drawing character. Each direction has two binary values: regular and bold. For up (1,2), right (4,8), down (16,32), and left (64,128)
linC :: Int -- ^ The given binary representation of the box-drawing character we wish to obtain.
    -> Char -- ^ The box-drawing character that corresponds to the given binary encoding.
linC   5 = '\x2514'
linC   6 = '\x2516'
linC   9 = '\x2515'
linC  10 = '\x2517'
linC  17 = '\x2502'
linC  20 = '\x250c'
linC  21 = '\x251c'
linC  22 = '\x252e'
linC  24 = '\x250d'
linC  25 = '\x251d'
linC  26 = '\x2521'
linC  31 = '\x2522'
linC  34 = '\x2503'
linC  36 = '\x250e'
linC  37 = '\x251f'
linC  38 = '\x2520'
linC  40 = '\x250f'
linC  42 = '\x2523'
linC  65 = '\x2518'
linC  66 = '\x251a'
linC  68 = '\x2500'
linC  69 = '\x2534'
linC  70 = '\x2538'
linC  73 = '\x2536'
linC  74 = '\x253a'
linC  80 = '\x2510'
linC  81 = '\x2524'
linC  82 = '\x2526'
linC  84 = '\x252c'
linC  85 = '\x253c'
linC  86 = '\x2540'
linC  88 = '\x252e'
linC  89 = '\x253e'
linC  90 = '\x2544'
linC  96 = '\x2512'
linC  97 = '\x2527'
linC  98 = '\x2528'
linC 100 = '\x2530'
linC 101 = '\x2541'
linC 102 = '\x2542'
linC 104 = '\x2532'
linC 105 = '\x2546'
linC 106 = '\x254a'
linC 129 = '\x2519'
linC 130 = '\x251b'
linC 133 = '\x2535'
linC 134 = '\x2539'
linC 136 = '\x2501'
linC 137 = '\x2537'
linC 138 = '\x253b'
linC 144 = '\x2511'
linC 145 = '\x2525'
linC 146 = '\x2529'
linC 148 = '\x252d'
linC 149 = '\x253d'
linC 150 = '\x2543'
linC 152 = '\x252f'
linC 153 = '\x253f'
linC 154 = '\x2547'
linC 160 = '\x2513'
linC 161 = '\x252a'
linC 162 = '\x252b'
linC 164 = '\x2531'
linC 165 = '\x2545'
linC 166 = '\x2549'
linC 168 = '\x2533'
linC 169 = '\x2548'
linC 170 = '\x254b'
linC _   = ' '

flld :: Int -> Char
flld 0 = ' '
flld 1 = '\x2591'
flld 2 = '\x2592'
flld _ = '\x2588'

-- | Get the X-coordinate of the list of `UiDecorator`s.
uiCurX :: [UiDecorator] -- ^ The given list of `UiDecorator`s to search in.
    -> Int -- ^ The resulting X-coordinate found in the list.
uiCurX (l@CursorX {}:_) = cursorX l
uiCurX (_:ls) = uiCurX ls
uiCurX _ = error "The decorators do not contain the X-coordinate for the cursor."

-- | Get the Y-coordinate of the list of `UiDecorator`s.
uiCurY :: [UiDecorator] -- ^ The given list of `UiDecorator`s to search in.
    -> Int -- ^ The resulting Y-coordinate found in the list.
uiCurY (l@CursorY {}:_) = cursorY l
uiCurY (_:ls) = uiCurY ls
uiCurY _ = error "The decorators do not contain the Y-coordinate for the cursur."

uiOpt :: Int -> [UiDecorator] -> Bool
uiOpt a (l@Option {}:_) | a == aim l = toggle l
uiOpt a (_:ls) = uiOpt a ls
uiOpt _ _ = error "The decorators do not contain the queried boolean option."

instance KeyContextHandler BitTh a where
    handleKeyCtx (KChar '0') _ _ _ = Just F
    handleKeyCtx (KChar 'F') _ _ _ = Just F
    handleKeyCtx (KChar 'f') _ _ _ = Just F
    handleKeyCtx (KChar '1') _ _ _ = Just T
    handleKeyCtx (KChar 'T') _ _ _ = Just T
    handleKeyCtx (KChar 't') _ _ _ = Just T
    handleKeyCtx (KChar '-') _ _ _ = Just D
    handleKeyCtx (KChar 'D') _ _ _ = Just D
    handleKeyCtx (KChar 'd') _ _ _ = Just D
    handleKeyCtx (KChar 'X') _ _ _ = Just D
    handleKeyCtx (KChar 'x') _ _ _ = Just D
    handleKeyCtx _           _ _ _ = Nothing

instance KeyContextHandler Bool a where
    handleKeyCtx (KChar '0') _ _ _ = Just False
    handleKeyCtx (KChar 'F') _ _ _ = Just False
    handleKeyCtx (KChar 'f') _ _ _ = Just False
    handleKeyCtx (KChar '1') _ _ _ = Just True
    handleKeyCtx (KChar 'T') _ _ _ = Just True
    handleKeyCtx (KChar 't') _ _ _ = Just True
    handleKeyCtx _           _ _ _ = Nothing

-- | A function that swaps the foreground and the background of the given attribute.
swapAttr :: Attr -- ^ The given attribute to process.
    -> Attr -- ^ The resulting attribute that is a copy of the given attribute except that the foreground and background are swapped.
swapAttr (Attr s fg bg) = Attr s bg fg

highlightColor :: Int -> Int
highlightColor 1 = 3
highlightColor 2 = 8
highlightColor 4 = 5
highlightColor x = x

-- | A utility function that produces for the given attribute and string a vertical string: the characters are appended in the vertical direction.
vstring :: Attr -- ^ The given attribute that determines how the string is rendered.
    -> String -- ^ The given string to be rendered in the vertical direction.
    -> Image -- ^ The resulting image that renders the string in the vertical direction.
vstring a = vertCat . map (char a)

class KeyContextHandler a ctx where
    handleKeyCtx :: Key -> [Modifier] -> ctx -> a -> Maybe a
    handleKeyCtx k m c x = Just $ handleKeyCtxId k m c x
    handleKeyCtxId :: Key -> [Modifier] -> ctx -> a -> a
    handleKeyCtxId k m c x | Just f <- handleKeyCtx k m c x = f
                           | otherwise = x

class WidgetKeyHandling a where
    handleKeyWidgetFallfront :: Widget a -> Key -> [Modifier] -> IO Bool
    handleKeyWidgetFallfront _ _ _ = return False
    handleKeyWidgetFallback :: Widget a -> Key -> [Modifier] -> IO Bool
    handleKeyWidgetFallback _ _ _ = return False

instance WidgetKeyHandling (Decorator a b)

handleKeyWidget :: (WidgetKeyHandling a,KeyContextHandler a a) => Widget a -> Key -> [Modifier] -> IO Bool
handleKeyWidget x y z = do
        ff <- handleKeyWidgetFallfront x y z
        if ff then return True else do
            s <- getState x
            r <- readIORef x
            f (focused r) (handleKeyCtx y z s s)
                where f True (Just l) = updateWidgetState x (const l) >> return True
                      f _    _        = handleKeyWidgetFallback x y z

bCursorX :: Int -> Int -> Int -> UiDecorator
bCursorX mn mx = CursorX mn mx . border mn mx

bCursorY :: Int -> Int -> Int -> UiDecorator
bCursorY mn mx = CursorY mn mx . border mn mx

instance KeyContextHandler UiDecorator a where
    handleKeyCtx KLeft  []      _ c@(CursorX mi _  a) | a > mi = Just $ c { cursorX = max mi (a-1) }
    handleKeyCtx KRight []      _ c@(CursorX _  ma a) | a < ma = Just $ c { cursorX = min ma (a+1) }
    handleKeyCtx KUp    []      _ c@(CursorY mi _  a) | a > mi = Just $ c { cursorY = max mi (a-1) }
    handleKeyCtx KDown  []      _ c@(CursorY _  ma a) | a < ma = Just $ c { cursorY = min ma (a+1) }
    handleKeyCtx ka     _       _ (Option d kb f) | ka == kb = Just $ Option (not d) kb f
    handleKeyCtx _      _       _ _ = Nothing

instance KeyContextHandler a ctx => KeyContextHandler [a] ctx where
    handleKeyCtx a b ctx (x:xs) | Just y <- handleKeyCtx a b ctx x = Just (y:xs)
                                | Just ys <- handleKeyCtx a b ctx xs = Just (x:ys)
    handleKeyCtx _ _ _   _ = Nothing

instance (KeyContextHandler a c,KeyContextHandler b c) => KeyContextHandler (Decorator a b) c where
    handleKeyCtx a b c x | Just de <- handleKeyCtx a b c $ decorators x = Just $ x { decorators = de }
                         | Just se <- handleKeyCtx a b c $ decorstate x = Just $ x { decorstate = se }
                         | otherwise = Nothing

-- | Replicate the image in both the column and row direction and perform a crop such that the image has a size as the given `DisplayRegion`. This can be used to fill an image according to a given pattern.
imageReplicate :: Image -- ^ The given pattern to be replicated.
    -> DisplayRegion -- ^ The given image size for the resulting image.
    -> Image -- ^ The resulting image that replicates the given pattern to the given `DisplayRegion`.
imageReplicate img (w,h) = crop w h $ replicateFoldl1 (<->) nh $ replicateFoldl1 (<|>) nw img
    where iw = imageWidth img
          ih = imageHeight img
          nw = div (w+iw-1) iw
          nh = div (h+ih-1) ih

alternate :: String -> (Image -> Image -> Image) -> Attr -> Int -> Image
alternate cs fl a n = foldr1 fl $ map (char a) $ take n $ cycle cs

identity :: Char -> Char -> Attr -> Int -> Image
identity c1 c0 a n = identity' 0
    where identity' i | i < n = string a (replicate i c0++c1:replicate (n-i-1) c0) <-> identity' (i+1)
                      | otherwise = emptyImage

matrixImg :: (Int -> Int -> Char) -> Attr -> Int -> Int -> Image
matrixImg f a w h = mi 0
    where mi i | i < h = string a (map (f i) [0..w-1]) <-> mi (i+1)
               | otherwise = emptyImage

genericFlatImage :: ([(a,b)] -> (a,b)) -> [(a,[[b]])] -> [[(a,b)]]
genericFlatImage fc = fi
    where fi ((_,[]):xs) = fi xs
          fi xs@(_:_) = fr (safeTmapH xs) : fi (safeTmapT xs)
          fi _ = []
          fr ((_,[]):xs) = fr xs
          fr xs@(_:_) = fc (safeTmapH xs) : fr (safeTmapT xs)
          fr _ = []
          safeTmapH ((x,h:_):xs) = (x,h) : safeTmapH xs
          safeTmapH (_:xs) = safeTmapH xs
          safeTmapH _ = []
          safeTmapT ((x,_:t):xs) = (x,t) : safeTmapT xs
          safeTmapT (_:xs) = safeTmapT xs
          safeTmapT _ = []

flatImg :: (b -> Bool) -> [(a,[[b]])] -> [[(a,b)]]
flatImg f = genericFlatImage fc
    where fc (ab@(_,bi):_:_) | f bi = ab
          fc (_:ab@(_:_)) = fc ab
          fc (ab:_) = ab
          fc [] = error "Cannot work on empty list"

flattenImg :: [(a,[String])] -> [[(a,Char)]]
flattenImg = flatImg (/= ' ')

composeImg :: [[(Attr,Char)]] -> Image
composeImg (x:xs) = foldr ((<|>) . uncurry char) emptyImage x <-> composeImg xs
composeImg _ = emptyImage

tileImg :: Attr -> [String] -> Image
tileImg a = ti
    where ti (x:xs) = foldr ((<|>) . char a) emptyImage x <-> ti xs
          ti _ = emptyImage

lineLabel :: [(String,Attr)] -> Image
lineLabel = foldr ((<->) . uncurry (flip string)) emptyImage

vlineILabel :: Attr -> Int -> [(Int,String,Attr)] -> Image
vlineILabel a0 w = vlil 0
    where vlil i ((j,s,a):ls) = string a0 (replicate (j-i) ' ') <|> vstring a s <|> vlil (j+1) ls
          vlil i [] = string a0 $ replicate (w-i) ' '
     

tapline :: Attr -> Attr -> Int -> [Int] -> [WireC] -> Image
tapline ad at n = t0 0
    where tg _  _  _  _  i _      _           | i > n = emptyImage
          tg _  _  _  cb i (t:ts) cs          | i == t = char at cb <|> t1 (i+1) ts cs
          tg cl _  _  _  i ts     ((c,ac):cs) | i == c = char ac '\x2502' <|> cl (i+1) ts cs
          tg cl au ca _  i ts     cs          = char au ca <|> cl (i+1) ts cs
          t0 = tg t0 ad ' ' '\x250c'
          t1 = tg t1 at '\x2500' '\x252c'

taplines :: Attr -> Int -> [([Int],Attr)] -> Image
taplines a w = tl' []
    where tl' hs ((ts,sa):os) = tapline a sa w ts hs <-> tl' (mergeOrdOn fst hs $ mapWithf (const sa) ts) os
          tl' _  [] = emptyImage

inboxH :: (Image -> Image -> Image) -> Attr -> [(Attr,[a])] -> (Char -> (Int -> String) -> Image,[[(a,Int)]])
inboxH imgr a l = ih' l 0
    where ih' :: [(Attr,[a])] -> Int -> (Char -> (Int -> String) -> Image,[[(a,Int)]])
          ih' ((ai,xs):xss) w | n > 1 = tl 1 n2 $ const $ \f -> string ai $ f n
                              | n < 1 = second ([]:) $ rec w
                              | otherwise = tl 0 1 $ \ca -> const $ char ai ca
              where n = length xs
                    n2 = n+2
                    rec =  ih' xss
                    tl d0 d1 bb = $(fTup 2) ($(overMN 2 2) imgr bb) (mi (w+d0):) $ rec (w+d1)
                    mi w0 = zip xs [w0..]
          ih' [] _ = ($(constN 2) $ emptyImage,[])

mapImg :: (Int -> Image) -> (Image -> Image -> Image) -> (a -> Image) -> [(a,Int)] -> Image
mapImg spg mgr gen lst = mi lst 0
    where mi ((x,c):xs) w = mgr (mgr (spg $ c-w) $ gen x) $ mi xs (c+1)
          mi [] _ = emptyImage

calcRout :: [WireR] -> ([WireC],[[WireR]],[WireC])
calcRout xs = fr [] (fastSortByOn (negate . minimum . fst) xs) [] []
    where hxs = map (head . fst) xs
          bvs = 1 + maximum hxs
          bvz = zeros $ bvs+length xs
          fr :: [WireC] -> [WireR] -> [(Int,BV,BV,[WireR])] -> [WireR] -> ([WireC],[[WireR]],[WireC])
                                                 -- TODO: resolve failures
          fr cts ((lb@(l@(x:ys),la)):rs) rti fld | sp0 == sp1 = fr ((x,la):cts) rs rti fld
                                                 | otherwise = uncurry (fr cts rs) safinj
              where sp0 = minimum l
                    sp1 = maximum l
                    iinj = shiftL (1 :: Integer)
                    mask = bitVec bvs $ sum $ map iinj ys
                    musk = bitVec bvs $ iinj x
                    safinj | Just sti <- stackinj l mask musk lb rti = (sti,fld)
                           | otherwise = (rti,lb:fld)
          fr _   (    ([]      ,_ ) :_ ) _   _ = error "All wires should have at least one input and one output pin."
          fr cts []                      rti fld = (cts,reverse $ map $(selN 4 3) $ fixinj ferr [] rti,sortOn fst $ map (first head) xs)
              where ferr = fastSortByOn (negate . minimum . fst) fld
          stackinj l mask musk lb = rtinj
              where sp0 = minimum l
                    sp1 = maximum l
                    rtinj lz@((t1,tp,tu,ls):ts) | tu .&. musk /= bvz = Nothing
                                                | tp .&. mask /= bvz = Just $ (sp0,musk,mask,[lb]):lz
                                                | t1 > sp1 = Just $ (sp0,tp .|. musk,tu .|. mask,lb:ls):ts
                                                | Just tlj <- tl = Just $ (t1,tp,tu .|. mask,ls) : tlj
                                                | otherwise = Nothing
                        where tl = rtinj ts
                    rtinj [] = Just [(sp0,musk,mask,[lb])]
          fixinj ((lb@(l@(x:ys),la)):rs) heap stack = fixinj rs heap stack --TODO: add failing lines to stack and heap
          fixinj []                      heap stack = stack++heap
          fixinj ((    []      ,_)  :_)  _    _ = error "All wires should have at least one input and one output pin."

routImg :: Attr -> Int -> ([WireC],[[WireR]],[WireC]) -> Image
routImg atr w (x1,x2,x3) = ri x1 x2 x3
    where ri :: [WireC] -> [[WireR]] -> [WireC] -> Image
          ri d (l:ls) du = imm <-> ri (mergeOrdOn fst d $ concatMap (\(lin,ain) -> map (\x -> (x,ain)) $ tail lin) l) ls dum
              where (dum,imm) = ril (mergeOrdOn fst d du) du 0 l
          ri _ [] _ = emptyImage
          ril :: [WireC] -> [WireC] -> Int -> [WireR] -> ([WireC],Image)
          ril ta0 (tu@(tu0,_):tus) c sg@((sgx:_,_):_) | tu0 < sgx = (tu:tu1,ti0)
              where (tu1,ti0) = ril ta0 tus c sg
          ril ta0 (_:tu0) c ((sg@(sgx:sgy),sga):sgs) = (tu1,ti0 <|> ti1 <|> ti2)
              where (ta1,ti0) = downtapDf atr ta0 c s0
                    (ta2,ti1) = rils sga ta1 s0 s0 s1 sgy sgx
                    (tu1,ti2) = ril ta2 tu0 (s1+1) sgs
                    s0 = minimum sg
                    s1 = maximum sg
          ril tap tup c [] = (tup,snd $ downtapDf atr tap c w)
          rils sga ta0 si s0 sj yo@(y:ys) x | x == y = segcha prl 3
                                            | mx == x = segcha prl 1
                                            | otherwise = segcha prl 2
              where prl | mx == si = 1
                        | mx == sj = 2
                        | otherwise = 0
                    mx = max s0 $ if x < s0 then y else min x y
                    (ta1,ima) = downtap '\x2502' '\x2500' sga ta0 s0 mx
                    (ta2,imb) = rils sga ta1 si (mx+1) sj (if y <= mx then ys else yo) x
                    segcha ta tb = (ta2,ima <|> char sga (picaH ta tb) <|> imb)
          rils sga ta0 _ s0 _ [] x | s0 < x = (ta1,ima <|> char sga (picaH 2 1))
                               | otherwise = (ta0,emptyImage)
              where (ta1,ima) = downtap '\x2502' '\x2500' sga ta0 s0 x

routMap :: Int -> ([WireC],[[WireR]],[WireC]) -> WreMp
routMap ww (a,b,c) = WreMp ww hh $ rut (map fst a) ($(mapN 2) fst b) (map fst c) 0 $ wreD $ zeroWireMap ww hh
    where rut _ []      _ _  mp = mp
          rut x ([]:ys) z ln mp = rut x ys z (ln+1) mp
          rut x ((ya:yb):ys) z ln mp = rut (x++tail ya) (yb:ys) z ln $ (UA.//) mp $ [(x2+xi,1)|xi<-x]++[(x2+xi,2)|xi<-[x0..x1]]++[(x2+xi,3)|xi<-ya]
              where x0 = minimum ya
                    x1 = maximum ya
                    x2 = ww*ln
          hh = length b

calcRoutImg :: Attr -> Int -> [WireR] -> Image
calcRoutImg atr w x = routImg atr w cr
    where cr = calcRout x

downtapDf :: Attr -> [(Int,Attr)] -> Int -> Int -> ([(Int,Attr)],Image)
downtapDf = downtap '\x2502' ' '

downtap :: Char -> Char -> Attr -> [(Int,Attr)] -> Int -> Int -> ([(Int,Attr)],Image)
downtap cdn csp atr pa xi x1 = dt pa xi
    where dt to@((t,ta):ts) x0 | t < x0 = dt ts x0
                               | t < x1 = (tr,replChr csp atr x0 t <|> char ta cdn <|> im)
                               | otherwise = (to,replChr csp atr x0 x1)
              where (tr,im) = dt ts (t+1)
          dt [] x0 = ([],replChr csp atr x0 x1)

replChr :: Char -> Attr -> Int -> Int -> Image
replChr c atr x0 x1 = foldr (<|>) emptyImage $ replicate (x1-x0) $ char atr c

spc :: Attr -> Int -> Int -> Image
spc = replChr ' '

mapHImg :: Attr -> (a -> Image) -> [(a,Int)] -> Image
mapHImg at = mapImg (string at . flip replicate ' ') (<|>)
