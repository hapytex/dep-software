{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, OverloadedStrings, UndecidableInstances #-}

module Dep.Printing (
        Table(..),
        mergeH,mergeV,                                                             --datas
        tableFSM,tableHtml5,
    ) where

import Control.Monad(forM_)

import Data.Bits
import Data.BitVector(BitVector,toBits)
import Data.Bool(bool)
import Data.Int
import Data.List (intercalate,transpose)
import Data.Function
import Data.Text(pack)

import Dep.Algorithms()
import Dep.Structures
import Dep.Utils(outersperse,stick,identityMatrix,tile,singleton,Table(..),Tabulatable(..),tableBar)

import System.Console.ANSI

import qualified Text.Blaze.Html5 as Ht
import qualified Text.Blaze.Html5.Attributes as At
import Text.Blaze.Html.Renderer.String(renderHtml)

default(String)

instance Mergeable Color where
    merge x y = Just $ fmByte $ on (.|.) toByte x y
        where toByte :: Color -> Int8
              toByte Black   = 0
              toByte Red     = 4
              toByte Green   = 2
              toByte Yellow  = 6
              toByte Blue    = 1
              toByte Magenta = 5
              toByte Cyan    = 3
              toByte White   = 7
              fmByte :: Int8 -> Color
              fmByte 0 = Black
              fmByte 4 = Red
              fmByte 2 = Green
              fmByte 6 = Yellow
              fmByte 1 = Blue
              fmByte 5 = Magenta
              fmByte 3 = Cyan
              fmByte _ = White

encodeLaTeX :: String -> String --TODO
encodeLaTeX [] = []
encodeLaTeX (c:cs) | c == '\n' = '\\' : '\\' : tl
                   | c == '\\' = "\\textescape" ++ tl
                   | c `elem` ("@%&" :: String) = '\\' : c : tl
                   | otherwise = c : tl
    where tl = encodeLaTeX cs

instance Printable Table where
    printAscii _ (Table dt hl _) = unlines $ injectHlines (tableBar $ transpose dt) hl -- TODO: Table with three parameters
    printSvg (Table dt hl _) = tableHtml5 "generic table" dt -- TODO: same
    printLaTeX (Table dt hl _) = tableLaTeX dt --TODO, TODO: same

injectHlines :: [String] -> [Int] -> [String]
injectHlines lls = ihl 0 lls
    where hl = replicate (maximum $ map length lls) '-'
          ihl i lz jz@(j:js) | j < i = ihl i lz js
                             | i == j = hl : ihl i lz js
                             | (l:ls) <- lz = l : ihl (i+1) ls jz
                             | otherwise = []
          ihl _ ls [] = ls

htmlDocument :: String -> Ht.Html -> String
htmlDocument title content = renderHtml $ Ht.docTypeHtml $ do
    Ht.head (Ht.title $ Ht.text $ pack title)
    Ht.body content

tableHtml5 :: String -> [[String]] -> String
tableHtml5 title dat = htmlDocument title $
    Ht.table $ forM_ dat (\row ->
        Ht.tr $ forM_ row $ Ht.td . Ht.code . Ht.text . pack
    )
    

tableLaTeX :: [[String]] -> String
tableLaTeX dt = "\\begin{tabular}{"++replicate (length $ head dt) 'c'++"}"++intercalate "\\\\" (map (intercalate "&" . map encodeLaTeX) dt)++"\\end{tabular}"


instance Tabulatable (FSM String BitThSeq BitThSeq) where
    toTable m = Table (tableFSM m) [1] [] -- TODO: constructor with three parameters
    fromTable = undefined

instance Printable (FSM String BitThSeq BitThSeq) where
    printAscii x = printAscii x . toTable
    printSvg = printSvg . toTable
    printLaTeX = printLaTeX . toTable

instance Printable BitVector where
    printAscii _ x = map b2c $ toBits x -- pr (bitSize x-1)
        where b2c True = '1'
              b2c False = '0'

instance Printable [BitVector] where
    printAscii d = unlines . map (printAscii d)

tableFSM (Moore st ip tf ef) = ("":map show ip++[""]):map showEntry st
    where showEntry s = (s:map (show . tf s) ip)++[show $ ef s]
tableFSM (Mealy st ip tf ef) = ("":map show ip):map showEntry st
    where showEntry s = s:map (\x -> show (tf s x) ++ '/' : show (ef s x)) ip

instance Show Driver where
    show ASCII = "ASCII - American Standard Code for Information Interchange"
    show SVG = "SVG/HTML - Structured Vector Graphic and Hyper Text Markup Language"
    show LaTeX = "LaTeX - Lamport TeX"

instance Show CombTable where
    show (CT n t) = unlines $ tableBar $ transpose $ showtab n [] t

showtab :: Int -> String -> Three BitThSeq -> [[String]]
showtab n x lv@(ThLeaf v) | n == 0 = [[reverse x,show v]]
                          | otherwise = showtab (n-1) ('-':x) lv
showtab n x (ThDirect a) = showtab (n-1) ('-':x) a
showtab n x (ThNode la lb) = showtab n1 ('0':x) la ++showtab n1 ('1':x) lb
     where n1 = n-1

instance Printable CombTable where
    printAscii _ = show
    printSvg (CT n t) = tableHtml5 "Combinatorial table" $ showtab n [] t

instance Printable BitThSeq where
    printAscii _ = show

instance (Show a) => Drawable (Three a) where
    draw n | (ThLeaf a) <- n = "o " ++ show a
           | (ThDirect a) <- n = "o\n" ++ drin [False] a
           | (ThNode a b) <- n = "o\n" ++ drin [True] a ++ "\n" ++ drin [False] b
        where simp :: Bool -> String
              simp = bool "`-o" "+-o"
              cplx :: Bool -> String
              cplx = bool "  " "| "
              pres :: [Bool] -> String
              pres (l:b) = prec b ++ simp l
              prec :: [Bool] -> String
              prec [] = ""
              prec (x:xs) = prec xs ++ cplx x
              drin :: Show a => [Bool] -> Three a -> String
              drin b  = (pres b ++) . drinp
                  where drinp (ThLeaf a) = ' ' : show a
                        drinp (ThDirect la) = '\n' : drin (False:b) la
                        drinp (ThNode la lb) = '\n' : drin (True:b) la ++ drinp (ThDirect lb)

instance Show CombElem where
    show (MinT its) = unwords $ map showElemItem its
    show (MaxT its) = intercalate " + " $ map showElemItem its
    show (SOP []) = show False
    show (SOP [[]]) = show True
    show (SOP its) = intercalate " + " $ map (show.MinT) its
    show (POS []) = show True
    show (POS [[]]) = show False
    show (POS its) = '(' : intercalate ") (" (map (show.MaxT) its)++")"
--TODO:   showList xs

instance Printable CombElem where
    printAscii _ = show

instance Printable [CombElem] where --TODO: modify
    printAscii _ = show

showElemItem :: Int -> String
showElemItem i | i > 0 = xi
               | otherwise = xi ++ "'"
               where xi = 'x' : show (abs i-1)

instance Show Karnaugh where
    show (KC t) = drawExtended t

instance Printable Karnaugh where
    printAscii w (KC t) = karnExtDraw w t

{-
instance (Show st,Show it,Show ot) => Show (FSM st it ot) where
    show (Moore ss is dt em) = unlines $ table (("State" : map show is ++ ["Output"] ) : map (\(Label x) -> show x : map (show . dt x) is++[show $ em x]) ss) [1]
    show (Mealy ss is dt em) = unlines $ table (("State" : map show is) : map (\(Label x) -> show x : map (\y -> show (dt x y)++"/"++show (em x y)) is) ss) [1]
-}

-- TODO: transform to width
instance DrawKarnaugh CombTable where
    drawCompact tab = concatMap (drawCompact . CF tab) [0..ysize tab-1]
    drawExtended = karnExtDraw Nothing


karnExtDraw w tab = unlines $ tile ' ' 4 2 w $ map (karn . CF tab) [0..ysize tab-1] -- printExtendedKK (head.show) "f" (\x -> 'x':(show x)) n t
        where karn (CF (CT n t) i) = printExtendedKK (head.show.drop i) ('f':show i) (\x -> 'x':show x) n t

instance DrawKarnaugh CombFunc where
    drawCompact (CF (CT n t) i) = printCompactKK (head.show.drop i) n t
    drawCompact (CFS n t i) = printCompactKK (head.show) n t
    drawExtended (CF (CT n t) i) = unlines $ printExtendedKK (head.show.drop i) ('f':show i) (\x -> 'x':show x) n t
    drawExtended (CFS n t i) = unlines $ printExtendedKK (head.show) ('f':show i) (\x -> 'x':show x) n t

printCompactKK :: (a -> Char) -> Int -> Three a -> String
printCompactKK p n l = unlines $ map (replicate (nh+1) ' ' ++) mw ++ [[]] ++ zipWith (\x y -> x++" "++y) mh (printCompactKK' p n l)
                         where nh = n `div` 2
                               nw = n-nh
                               mw = printMarks kkMarkerH (shiftL 1 (nw-1))
                               mh = transpose $ printMarks kkMarkerV (shiftL 1 (nh-1))

printExtendedKK :: (a -> Char) -> String -> (Int -> String) -> Int -> Three a -> [String]
printExtendedKK p fn xns n l = fn : (stem ++ zipWith (++) lm tbl)
                         where nh = n `div` 2
                               nw = n-nh
                               nm = max nw nh
                               tbl = outersperse hl $ map (outersperse '|') $ printCompactKK' p n l
                               stem = identityMatrix '\\' ' ' nm nm
                               lm = replicate (length tbl) $ replicate nm ' '
                               hl = '+' : take (shiftL 1 (nw+1)) (cycle "-+")

printCompactKK' :: (a -> Char) -> Int -> Three a -> [String]
printCompactKK' p n (ThLeaf l) | n == 0 = [[p l]]
                               | n `mod` 2 == 0 = pl++reverse pl
                               | otherwise = map (\x -> x++reverse x) pl
                               where pl = printCompactKK' p (n-1) (ThLeaf l)
printCompactKK' p n (ThDirect la) | n `mod` 2 == 0 = pl++reverse pl
                                  | otherwise = map (\x -> x++reverse x) pl
                                  where pl = printCompactKK' p (n-1) la
printCompactKK' p n (ThNode la lb) | n `mod` 2 == 0 = pla++reverse plb
                                   | otherwise = zipWith (\x y -> x++reverse y) pla plb
                                   where pla = printCompactKK' p n1 la
                                         plb = printCompactKK' p n1 lb
                                         n1 = n-1

printMarks :: (Int -> String) -> Int -> [String]
printMarks f = map (concatMap g) . printMarks'
    where g x | x > 0 = f x
              | otherwise = replicate (-x) ' '

printMarks' :: Int -> [[Int]]
printMarks' 0 = []
printMarks' n = [-n,n] : map (\x -> stick stickSign (x++reverse x)) (printMarks' (shiftR n 1))

stickSign :: Int -> Int -> Maybe Int
stickSign a b | m >= 0 = Just s
              | otherwise = Nothing
              where m=a*b
                    s=a+b

kkMarker :: a -> a -> a -> Int -> [a]
kkMarker ca cb cc n = ca : replicate (n-2) cb ++ [cc]

kkMarkerH :: Int -> String
kkMarkerH = kkMarker '<' '-' '>'

kkMarkerV :: Int -> String
kkMarkerV = kkMarker '^' '|' 'v'

mergeH :: [String] -> [String] -> [String]
mergeH la = mergeH' la
    where ca = maximum $ map length la
          mergeH' (l:ls) (r:rs) = (l ++ replicate (ca-cl) ' ' ++ r) : mergeH' ls rs
              where cl = length l
          mergeH' l [] = l
          mergeH' [] (r:rs) = (replicate ca ' ' ++ r) : mergeH' [] rs

mergeV :: [String] -> [String] -> [String]
mergeV = (++)
