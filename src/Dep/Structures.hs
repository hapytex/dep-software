{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

-- | A package describing the basic datastructures and classes that are used and manipulated by the rest of the program.
module Dep.Structures(
    Expandable(..),Reduceable(..),Drawable(..),BitLookup(..),Specifiable(..),   --classes
    DrawKarnaugh(..),Bitwise(..),Printable(..),Describeable(..),Mergeable(..),
    Assignable(..),
    ANSIShow(..),Serializeable(..),VarInv(..),Equivalent(..),EqBy(..),NmapM(..),
    BitTh(..),Three(..),CombFunc(..),CombTable(..),                             --data
    CombElem(..),
    Karnaugh(..),Driver(..),FSM(..),FSMState(..),Wavehead(..),
    BitThSeq,                                                                   --type
    sop,sopOrd,soplOrd,sopmOrd,                                                 --functions
) where

import Control.Monad(ap)

import Data.Aeson(ToJSON(toJSON),FromJSON(parseJSON), Value(Bool, Null))
import qualified Data.ByteString as BS
import qualified Data.BitVector as BV
import Data.Function(on)
import Data.Hashable(Hashable(hashWithSalt,hash))
import Data.Maybe(isJust, mapMaybe)
import Data.List(intercalate,sortBy)
import Data.Monoid(mappend)
import Data.String(IsString(fromString))
import Data.Word(Word8)
import Data.Bits(setBit)
-- import Data.Text(pack, unpack)

import Dep.Utils(maybeId,mapWithf,Composite(..),withf)

-- | A class that specifies that the object represents a function that maps a tuple of `xsize` elements to a tuple of `ysize` elements.
class NmapM a where
    -- | The number of input elements.
    xsize :: a -- ^ A typeholder item, necessary for type coercion.
        -> Int -- ^ The number of input elements corresponding to the function.
    xsize = const 0

    -- | The number of output elements.
    ysize :: a -- ^ A typeholder item, necessary for type coercion.
        -> Int -- ^ The number of output elements corresponding to the function.
    ysize = const 0

-- | A class specifying that elements of type `a` can be converted into equivalent elements of type `b` and vice versa.
class Equivalent a b where
    -- | The function that converts elements of type `a` to their equivalent type `b` counterparts.
    eqto :: a -- ^ The given element to convert.
        -> b -- ^ The resulting element of conversion.

    -- | The function that converts elements of type `b` to their equivalent type `a` counterparts.
    eqfrom :: b -- ^ The given element to convert.
        -> a -- ^ The resulting element of conversion.

    -- | The operator that performs an `eqto` function.
    (-~->) :: a -- ^ The given element to convert.
        -> b -- ^ The resulting element of conversion.
    (-~->) = eqto

    -- | The operator that performs the `eqfrom` function.
    (<-~-) :: b -- ^ The given element to convert.
        -> a -- ^ The resulting element of conversion.
    (<-~-) = eqfrom


-- | Determine whether the two datastructures `d` hold equivalent data. This means that the shape of the datastructure is equivalent in the leaves, there should  be equivalent data stored.
class EqBy d where
    -- | Test whether the the two datastructures `d a` and `d b` hold equivalent data.
    eqby :: (a -> b -> Bool) -- ^ The given equivalence function to use.
       -> d a -- ^ The first given datastructure to check.
       -> d b -- ^ The second given datastructure to check.
       -> Bool -- ^ True if the two datastructures hold equivalent data; False otherwise.

-- | A typeclass that specifies that the datatype can be expanded: the result is an equivalent data-structure but more uniform and perhaps faster.
class Expandable a where
    -- | Expand the given datastructure to a more uniform one.
    expand :: a -- ^ The given datastructure to expand.
        -> a -- ^ The resulting expanded datastructure.

-- | A typeclass that specifies that the datatype can be reduced: the result is an equivalent data-structure but more compact and therefore perhaps faster.
class Reduceable a where
    -- | Reduce the given datastructure to a more compact one.
    reduce :: a -> a -- ^ The resulting expanded data-structure.

-- | A typeclass that specifies that the given type can be converted into an
-- ASCII art drawing.
class Drawable a where
    -- | Convert the given object to an ASCII art drawing.
    draw :: a -- ^ The given object to draw.
         -> String -- ^ The resulting ASCII art.

-- | Specifies that the given datatype can be shown on an ANSI compatible terminal. The ANSI terminal provides utility functions such as colors, underline, blink to make data more accessible.
class ANSIShow a where
    -- | The equivalent of `show` but for an ANSI terminal.
    showAnsi :: a -- ^ The object to show on an ANSI terminal.
        -> String -- ^ The resulting string that may contain ANSI escape sequences.

    -- | Show a list of items on the ANSI terminal.
    showAnsiList :: [a] -- The ANSI terminal to render the list of elements on.
        -> ShowS -- ^ The resulting rendering function that shows all the elements.
    showAnsiList l = const $ '[' : intercalate "," (map showAnsi l)++"]"

instance (ANSIShow a) => ANSIShow [a] where
    showAnsi x = showAnsiList x []

-- | A typeclass that specifies that variables (denoted as integers) are
-- involved.
class VarInv a where
    -- | The list of variables involved in the object.
    involvedVar :: a -- ^ The object to analyze.
                -> [Int] -- ^ The resulting list of indices of the variables, should be ordered.

    -- | The maximum number of variables that are involved over this object.
    maxVars :: a -- ^ The object to analyze.
            -> Int -- ^ The index of the largest
    maxVars = last . involvedVar

-- | Specifies an assignable ordering function on the different elements of the datatype. If the two elements are equivalent, the first can definitely be assigned to the second.
class Assignable a where
    -- | Check if the first given item can be assigned to the second given item.
    canAssign :: a -- ^ The first given item, the source.
        -> a -- ^ The second given item, the drain.
        -> Bool -- ^ A boolean indicating that the source can be assigned to the drain.
    canAssign = canAssignTo

    -- | Check if the first given item can be assigned to the second given item.
    canAssignTo :: a -- ^ The first given item, the source.
        -> a -- ^ The second given item, the drain.
        -> Bool -- ^ A boolean indicating that the source can be assigned to the drain.
    canAssignTo = flip canAssignFrom

    -- | Check if the first given item can be assigned from the second given item.
    canAssignFrom :: a -- ^ The first given item, the drain.
        -> a -- ^ The second given item, the source.
        -> Bool -- ^ A boolean indicating that the source can be assigned to the drain.
    canAssignFrom = flip canAssignTo

class Specifiable ds ix it where
    specify :: ix -> it -> ds -> ds
    specify ix = specifyMod ix . const
    specifyMod :: ix -> (it -> it) -> ds -> ds
    specifyMod ix f = specifyModMay ix (Just . f)
    specifyModMay :: ix -> (it -> Maybe it) -> ds -> ds
    specifyModMay ix f = specifyMod ix (maybeId f)

class Printable a where
    printAscii :: Maybe Int -> a -> String
    printSvg :: a -> String
    printSvg = printAscii Nothing
    printLaTeX :: a -> String
    printLaTeX = printAscii Nothing
    print :: Driver -> Maybe Int -> a -> String
    print ASCII d = printAscii d
    print SVG _ = printSvg
    print LaTeX _ = printLaTeX
    print _ d = printAscii d --TODO: find solution for bin

class Show a => Describeable a where
    description :: a -> String
    namedescribe :: a -> String
    namedescribe x | null sy = sx
                   | otherwise = sx ++ " - " ++ sy
                   where sx = show x
                         sy = description x

class BitLookup a b where
    btlookupTh :: [BitTh] -> a -> Maybe b
    btlookupThJust :: [BitTh] -> a -> b
    btlookupThJust b x | Just f <- btlookupTh b x = f
                       | otherwise = error "The queried bitstring results in multiple results."
    blookup :: [Bool] -> a -> b
    blookup b x | Just j <- btlookupTh (map convert b) x = j
                | otherwise = error "The querified bitstring results in multiple results."

class DrawKarnaugh a where
    drawCompact :: a -> String
    drawCompact _ = error "Compact mode not supported!"
    drawExtended :: a -> String
    drawExtended _ = error "Extended mode not supported!"
    drawSvg :: a -> String
    drawSvg _ = error "SVG mode not supported!"
    drawLaTeX :: a -> String
    drawLaTeX _ = error "LaTeX mode not supported!"

class Mergeable a where
    -- | Merge the two given values into a new value. Nothing if the values can
    -- not be merged, Just x otherwise.
    merge :: a -- ^ The first operand to merge.
          -> a -- ^ The second operand to merge.
          -> Maybe a -- ^ The outcome of the merge: Just x if the elements can merged, Nothing otherwise.

    -- | Determine if the two given operands can be merged. True if they can,
    -- False otherwise
    canMerge :: a -- ^ The first operand to check.
             -> a -- ^ The second operand to check.
             -> Bool -- ^ True if the operands can be merged, False otherwise.
    canMerge x = isJust . merge x

-- | Specify that one can perform bitwise operations on the datastructure like and, or, negation and xor.
class Bitwise a where
    -- | Calculate the bitwise negation.
    neg :: a -- ^ The given element to calculate the bitwise negation from.
        -> a -- ^ The resulting element that is the bitwise negation of the given element.

    -- | Calculate the bitwise AND operation.
    (.&) :: a -- ^ The given first element to calculate the bitwise AND operation from.
        -> a -- ^ The given second element to calculate the bitwise AND operation from.
        -> a -- ^ The result of the bitwise AND operation.
    (.&) x y = neg (neg x .| neg y)

    -- | Calculate the bitwise OR operation.
    (.|) :: a -- ^ The given first element to calculate the bitwise OR operation from.
        -> a -- ^ The given second element to calculate the bitwise OR operation from.
        -> a -- ^ The result of the bitwise OR operation.
    (.|) x y = neg (neg x .& neg y)

    -- | Calculate the bitwise XOR operation.
    (.^) :: a -- ^ The given first element to calculate the bitwise XOR operation from.
        -> a -- ^ The given second element to calculate the bitwise XOR operation from.
        -> a -- ^ The result of the bitwise XOR operation.
    (.^) x y = (neg x .& y) .| (x .& neg y)

-- | If a type `a` is bitwise, a list of `a` is bitwise as well by applying the operation on all elements.
instance Bitwise a => Bitwise [a] where
    -- | The negation of a list of bitwise elements is the mapping of the
    -- negation.
    neg = map neg
    -- | The logical and of two btwise lists is the elementwise logical and of
    -- the elements.
    (.&) = zipWith (.&)
    -- | The logical or of two bitwise lists is the elementwise logical or of
    -- the elements.
    (.|) = zipWith (.|)
    -- | The logical xor of two bitwise lists is the elementwise logical xor of
    -- the elements.
    (.^) = zipWith (.^)

-- | A three state bit: False, True and Don't Care/Don't Know
data BitTh = F -- ^ The False state of the three state logic.
    | D -- ^ The Don't Care/Don't Know state of the three state logic.
    | T -- ^ The True state of the three state logic.
    deriving (Eq,Enum,Bounded,Ord)

-- | Make a three state bit hashable.
instance Hashable BitTh where
    -- | In order to calculate the hash, we use the enumeration valuse of the
    -- three state byte.
    hashWithSalt i = hashWithSalt i . fromEnum
    -- | In order to calculate the hash, we use the enumeration value of the
    -- three state byte.
    hash = hash . fromEnum

-- | Turn a textual character into its three state boolean equivalent.
readBitTh :: Char -- ^ The character that is parsed
          -> Maybe BitTh -- ^ The BitTh value that corresponds to the given character.
readBitTh '0' = Just F
readBitTh 'F' = Just F
readBitTh 'f' = Just F
readBitTh '-' = Just D
readBitTh 'D' = Just D
readBitTh 'd' = Just D
readBitTh 'X' = Just D
readBitTh 'x' = Just D
readBitTh '1' = Just T
readBitTh 'T' = Just T
readBitTh 't' = Just T
readBitTh _ = Nothing

instance Read BitTh where
    readsPrec 0 (x:xs) | Just r <- readBitTh x = [(r, xs)]
                       | otherwise = error ("Can not decode the character '" ++ x : "' into a three state logic bit.")
    readsPrec _ x = error $ "Cannot convert a string \""++x++"\" to a three state logic bit."
    readList x = [(mapMaybe snd xa, map fst xb)]
        where (xa,xb) = span (isJust . snd) (map (ap (,) readBitTh) x)

instance IsString BitTh where
    fromString = read

instance IsString [BitTh] where
    fromString = read

-- | The driver that produces output for the given problem. Each driver will result in a different kind of output format.
data Driver = ASCII -- ^ The format is represented in ASCII (art) to the user.
    | SVG -- ^ The format is a *Scalable Graphics Format* file.
    | LaTeX -- ^ The format is a LaTeX file.
    | Bin -- ^ The format is a binary file.
    deriving (Enum,Eq,Bounded,Ord)

-- | A datastructure that is the generalization of a tree with three possible nodes: leaves, inodes and direct nodes.
data Three a = ThLeaf {thval :: a} -- ^ The leave node: contains a value and has no children.
    | ThNode {thleft :: Three a, thright :: Three a} -- The inode, contains a False child and a True child.
    | ThDirect {thdir :: Three a} -- The direct node: an inode but where both children are equal (by reference).

instance ToJSON a => ToJSON (Three a) where
    toJSON = undefined -- TODO

-- | A combinatorial table that has a number of variables, as well as a Three
-- structure to perform lookups and processing.
data CombTable = CT { ctnvar :: Int -- ^ The number of variables involved in the combinatorial table.
                    , cttab :: Three BitThSeq -- ^ The three structure that describes the combinatorial function.
                    }

-- | A combinatorial function, that is a combinatorial table, where we only
-- consider one output variable.
data CombFunc = CF CombTable Int
              | CFS Int (Three BitTh) Int

newtype Karnaugh = KC CombTable

data CombElem = MinT [Int] | MaxT [Int] | SOP {terms :: [[Int]]} | POS {terms::[[Int]]}

-- | An enumeration of possible encodings for a finite state machine.
data Encoding = OneHot -- ^ The one hot encoding where at each moment in time, one flipflop is high and the others low.
    | Binary -- ^ The binary encoding where, depending on the state each binary number is attached to a state.
    | Gray -- ^ The gray counter encoding where for each transition in the sequence, exactly two bits change.
    | Minimum -- ^ The minimum encoding, searched for by trying all possible combinations (can be unfeasible).
    | MinimalGreedy -- ^ The minimal encoding acording to a greedy heuristic.
    deriving (Show,Eq,Read,Enum,Ord)

-- | A three is a composite-type datastructure, it has children and thus we can benefit from utility functions.
instance Show a => Composite (Three a) where
     composite_children (ThLeaf _) = []
     composite_children (ThNode l r) = [l,r]
     composite_children (ThDirect l) = [l]
     composite_print_item (ThLeaf x) = 'L':show x
     composite_print_item (ThNode _ _) = "N"
     composite_print_item (ThDirect _) = "D"

-- | The default way to show a three is its tree-like ASCII art representation. Helps best when debugging.
instance Show a => Show (Three a) where
     show = composite_treeprint

-- CombElem constructors
sop :: [[Int]] -> CombElem
sop = SOP . sopmOrd

sopmOrd :: [[Int]] -> [[Int]]
sopmOrd = map fst . sortBy soplOrd . mapWithf length

sopOrd :: [Int] -> [Int] -> Ordering
sopOrd = on soplOrd (withf length)

soplOrd :: ([Int],Int) -> ([Int],Int) -> Ordering
soplOrd (xa,la) (xb,lb) = mappend (compare la lb) (compare xa xb)

-- | The textual representation of a BitTh is an 0 (False), 1 (True) and -
-- (don't care)
instance Show BitTh where
    show F = "0"
    show D = "-"
    show T = "1"
    showList l = const $ concatMap show l


-- | Enable converting a three-valued boolean value to a JSON string object.
instance ToJSON BitTh where
    toJSON T = Bool True
    toJSON F = Bool False
    toJSON D = Null

-- | Allow parsing a JSON string object to a thee-valued boolean object.
instance FromJSON BitTh where
    parseJSON (Bool True) = return T
    parseJSON (Bool False) = return F
    parseJSON Null = return D
    parseJSON _ = fail "Unable to convert the JSON element to a three-valued boolean."

convert :: Bool -> BitTh
convert True = T
convert False = F

donvert :: BitTh -> Bool
donvert T = True
donvert F = False
donvert D = error "Can not convert to a boolean"

-- | A three-state logic bit sequence is a sequence (list) of three state bits.
type BitThSeq = [BitTh]

-- | A finite state machine state is either an identifier for the type parameter `a`, or a don't care.
data FSMState a = Label {
                       stateLabel :: a -- ^ The label for the given state.
                      } -- ^ A label for a finite state machine.
                | DontCare -- ^ We do not care about the next label.
                deriving Eq

instance Show (FSMState String) where
    show (Label a) = a
    show DontCare = "-"
    showList [] = id
    showList xs = const $ intercalate ", " $ map show xs

instance Show (FSMState BitThSeq) where
    show(Label a) = show a
    show DontCare = "-"
    showList [] = id
    showList xs = const $ intercalate ", " $ map show xs

instance Hashable a => Hashable (FSMState a) where
    hashWithSalt s (Label l) = hashWithSalt s l
    hashWithSalt _ DontCare = 0

-- | A finite state machine. The type parameters are the type of states, the type of input and the type of output.
data FSM st it ot =
    Moore {
        states :: [st]
       ,inputs :: [it]
       ,transit :: st -> it -> FSMState st
       ,emitMo :: st -> ot
    } | Mealy {
        states :: [st]
       ,inputs :: [it]
       ,transit :: st -> it -> FSMState st
       ,emitMy :: st -> it -> ot
    }

-- | A structure describing a part of how the states of wire evolve over time.
data Wavehead = Wavehead { wireNames :: [String] -- ^ The name of the wires, is finite and equal to the size of the bitvectors in `wireStates`.
                         , wireStates :: [(Int,BV.BitVector)] -- ^ The list of bitvectors over time. Can be infinite (although not likely). The first element of the tuple is the time and must be ascending, the second element is the bitvector at that time.
                         } deriving Show

-- | A typeclass that specifies that the datastructure can be converted to sequence of `Word16`s, booleans or `BitTh`s.
class Serializeable a where
    -- | Convert the given object to a sequence of `BitTh`s that describe the object.
    serialize :: a -- ^ The object that should be serialized.
        -> [BitTh] -- ^ The resulting sequence of `BitTh` values that describe the object to serialize.

    -- | Convert the given object to a sequence of `Bool`s that describe the object.
    serializeBool :: a -- ^ The object that should be serialized.
        -> [Bool] -- ^ The resulting sequence of `Bool` values that describe the object to serialize.
    serializeBool = map donvert . serialize

    -- | Convert the given object to a sequence of `Word8`s that describe the object.
    serializeWord :: a -- ^ The object that should be serialized.
        -> [Word8] -- ^ The resulting sequence of `Word8` values that describe the object to serialize.
    serializeWord = pack . serialize
        where pack :: [BitTh] -> [Word8]
              pack [] = []
              pack l = v : pack t
                  where (t,v) = packWord8 7 l 0
              packWord8 :: Int -> [BitTh] -> Word8 -> ([BitTh],Word8)
              packWord8 _ [] = ([],)
              packWord8 n xl@(x:xs) | n < 0 = (xl,)
                                    | T <- x = pw8 . flip setBit n
                                    | otherwise = pw8
                  where pw8 = packWord8 (n-1) xs

    -- | Convert the given object to a ByteStream that describes the object.
    serializeByteString :: a -- ^ The object that should be serialized.
        -> BS.ByteString -- ^ The resulting BitString that describe the object to serialize.
    serializeByteString = BS.pack . serializeWord

{-
instance Bits a => Serializeable a where
    serialize v = ser $ (bitSize v)-1
        where ser n | n < 0 = []
                    | testBit v n = T : tl
                    | otherwise = F : tl
                  where tl = ser $ n-1
-}
