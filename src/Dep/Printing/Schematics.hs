module Dep.Printing.Schematics where

import Data.Bits
import Data.Word
import Dep.Structures(CombElem(..))
import Dep.Utils (safeTail,safeMapHeads,findDefault,succR,predR,nSuccR)

data Direction = U | R | D | L deriving (Enum,Show,Eq,Bounded)
data Wire = Space | End Direction | VLine | HLine | Cross | TSplit Direction | Turn Direction deriving (Eq)
--Turn contains the highest direction

shiftAdd :: (Bits b,Num b) => Int -> b -> b -> b
shiftAdd s m x = (x .&. m)+(shiftR x s .&. m)

countSet :: Word8 -> Word8
countSet = shiftAdd 4 15 . shiftAdd 2 51 . shiftAdd 1 85

printGate :: [Bool] -> Bool -> Char -> [String]
printGate li lo gt = (' ':'\x250c': replicate ll '\x2500' ++ "\x256e") : printGate' l2 ll li lo gt
    where ll = length li
          l2 = div ll 2

printGate' :: Int -> Int -> [Bool] -> Bool -> Char -> [String]
printGate' _ g [] _ _ = [' ':'\x2514': replicate g '\x2500' ++ "\x256f" ]
printGate' l2 g [i] o s = printGateLine l2 g i o s : printGate' l2 g [] o s
printGate' l2 g (i:is) o s = printGateLine l2 g i o s : printGate' (l2-1) g is o s

printGateLine :: Int -> Int -> Bool -> Bool -> Char -> String
printGateLine 1 g i o s = printIW i : '\x2524' : replicate g s ++ ['\x251c',printIW o]
printGateLine _ g i _ s = printIW i : '\x2524' : replicate g s ++ "\x2502"

printIW :: Bool -> Char
printIW False = '\x2500'
printIW True = 'o'

--{-
instance Show Wire where
    show (End _) = "."
    show HLine = "\x2500"
    show VLine = "\x2502"
    show Cross = "\x253c"
    show (TSplit U) = "\x2534"
    show (TSplit R) = "\x251c"
    show (TSplit D) = "\x2530"
    show (TSplit _) = "\x2524"
    show (Turn U) = "\x2518" --TODO: decide how to turn
    show (Turn R) = "\x2514"
    show (Turn D) = "\x250c"
    show (Turn _) = "\x2510"
    show _ = " "
--}

dirMask :: Direction -> Word8
dirMask = shiftL 1 . fromEnum

maskDir :: Word8 -> Direction
maskDir 1 = U
maskDir 2 = R
maskDir 4 = D
maskDir _ = L

wireMask :: Wire -> Word8
wireMask (End d) = dirMask d
wireMask (Turn d) = dirMask d .|. dirMask (predR d)
wireMask (TSplit d) = wireMask (Turn d) .|. dirMask (succR d)
wireMask VLine = dirMask U .|. dirMask D
wireMask HLine = dirMask L .|. dirMask R
wireMask Cross = 15
wireMask _ = 0

maskWire :: Word8 -> Wire
maskWire x = maskWire' s x
           where s = countSet x

maskWire' :: Word8 -> Word8 -> Wire
maskWire' 2 x | x == 5 = VLine
              | x == 10 = HLine
              | x == 3 = Turn R
              | x == 6 = Turn D
              | x == 12 = Turn L
              | otherwise = Turn U--9
maskWire' 3 x = TSplit $ nSuccR 2 $ maskDir $ 15 .&. complement x
maskWire' 4 _ = Cross
maskWire' 1 x = End $ maskDir x
maskWire' _ _ = Space

--instance Bits Wire where

stackedPrint :: [[String]] -> [String]
stackedPrint = genericStackedPrint ' ' (/= ' ')

genericStackedPrint :: a -> (a -> Bool) -> [[[a]]] -> [[a]]
genericStackedPrint dflt flld lyrs = map (map (findDefault dflt flld)) st
    where st = stackTranspose lyrs

--Permutates the dimensions such that the element for (i,j,k) is mapped to (j,k,i)
stackTranspose :: [[[a]]] -> [[[a]]]
stackTranspose [] = []
stackTranspose ([]:xs) = stackTranspose xs
stackTranspose xs = stackTransposeRow (safeMapHeads xs) : stackTranspose (map safeTail xs)

stackTransposeRow :: [[a]] -> [[a]]
stackTransposeRow [] = []
stackTransposeRow ([]:xs) = stackTransposeRow xs
stackTransposeRow xs = safeMapHeads xs : stackTransposeRow (map safeTail xs)

printCombElem :: CombElem -> [String]
printCombElem (MinT _) = []
printCombElem _ = []
-- | MaxT [Int] | SOP [[Int]] | POS [[Int]]
