{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A module that contains utility functions for the program. The utility functions perform all kinds of tasks.
module Dep.Utils (stick,fixpoint,scanFixpoint,outersperse,matrix,identityMatrix,             -- functions
        comparator, comparatorWith, Comparator(..),
        levenshtein,argmax,argmin,argminBy,argmaxBy,
        side,genericSide,tile,sideBar,tableBar,(<&|>),(<&->),equalWidth,
        singleGroupBy,steps,steps0,mapSteps,allSteps,singleton,specifyMerge,
        tupleMap,
        unroll,
        hashItemList,hashItemLists,hashItemIndex,hashGenerator,
        maybeId,maybenize,similarity,
        varRg,(.*),
        groupF,funTup,
        safeTail,safeInit,safeMapHeads,
        succR,predR,nF,nSuccR,nPredR,
        findDefault,genericBorder,border,retryMaybe,
        setFst,setSnd,withf,fTup,fTimes,fTimesEq,
        mapWithf,mapN,selN,curryN,uncurryN,mapred,
        ordNub,ordNubBy,mergeOrd,mergeOrdBy,mergeOrdOn,subOrd,subOrdBy,
        selectBool,selectNum,
        distribution,distribution100,
        replicateFoldl1,concatReplicate,replicateItems,burstItems,genericReplicateItems,
        burst,burstInner,
        adjustsSq,
        commutative,
        genericFastSortByOn,fastSortByOn,sortOn,
        overMN,constN,
        dijkstra,
        cmpThenOn,
        spread,spread0,spreadh,spread0h,
        showBinary,
        filterAnsi,
        alternateSeq,negAlternateSeq,
        trimStringLeft,trimStringRight,trimString,
        grayInc,
        powerl,
        Table(..),                                                              -- datas
        Composite(..),
        Tabulatable(..),
        Truthfull(..),
        listIndices,
        nand, nor, xoring, xnoring
    ) where

import Control.Arrow(first,second)
import Control.Monad(replicateM,ap)

import Data.Bits(Bits(testBit,complement,popCount,shiftL,(.&.),xor,complementBit),FiniteBits(finiteBitSize))
import Data.Char(isSpace)
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Heap as He
import Data.List(sortBy,nub,find,transpose,dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(mempty, mappend))
import qualified Data.Sequence as Sq

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Converting the list of indices to a list of values with a given value in
-- case the index is not mentioned and one in case the index is mentioned.
listIndices :: a -- ^ The value in case the item is not mentioned.
            -> a -- ^ The value in case the item is mentioned.
            -> [Int] -- ^ The list of indices, must be sorted.
            -> [a] -- ^ A list of values with as length the maximum index plus one.
listIndices v0 v1 = go 0
    where go i (x:xs) = replicate (x-i) v0 ++ v1 : go (x+1) xs
          go _ [] = []

-- | A comparator takes two input values and returns the ordering between the
-- two.
newtype Comparator a = Comparator (a -> a -> Ordering)

-- | Constructs a comparator of the ordering type.
comparator :: Ord a => Comparator a
comparator = Comparator compare

-- | Constructs a comparator by channeling the inputs through a given function.
comparatorWith :: Ord b => (a -> b) -- ^ The function to map the input through.
               -> Comparator a -- ^ The resulting comparator.
comparatorWith f = Comparator (on compare f)

instance Semigroup (Comparator a) where
    -- | By mappending the two comparisons, we obtain a comparator that calls
    -- the two comparators and mappends the results.
    (Comparator f) <> (Comparator g) = Comparator (\x y -> f x y <> g x y)

-- | A comparator is a monoid with a constant equality as empty, and appending
-- mappends the outcome of the comparisons.
instance Monoid (Comparator a) where
    -- | The mempty of a comparator takes for all inputs EQ.
    mempty = Comparator (const (const EQ))


-- | A class that describes a *composite* pattern: a pattern where an element can have children that have the same type. The class provides utility functions for composites such as printing the tree and determining the leaves and descendants.
class Composite a where
    -- | A function that generates a list of children of the given element. Note that the children do not have to be materialized: they can be generated on the fly by a generator. The default is returning an empty list.
    composite_children :: a -- ^ The given element to generate the children from.
        -> [a] -- ^ The list of children of the given element, by default the empty list.
    composite_children = const []

    -- | A function that determines - based on the `composite_children` function whether the given element has children. This is done in a lazy way such that for infinite list generators, the program does not get stuck in an infinite loop.
    composite_has_children :: a -- ^ The given element to determine whether it has children.
        -> Bool -- ^ The result: True if the given element has children, False otherwise.
    composite_has_children = not . null . composite_children

    -- | Generate a list of all descendants of a given item. The item is considered to be a descendant of oneself.
    composite_descendants :: a -- ^ The given item to generate all the descendants from.
        -> [a] -- ^ The resulting list of all descendants (the item itself included).
    composite_descendants = cd []
        where cd tl x = x : foldl cd tl (composite_children x)

    -- | Generate a list of all leaves that originate from the given item. If the given item is a leave, a list containing
    -- only the item is returned.
    composite_leaves :: a -- ^ The given item to generate a list of leaves from.
        -> [a] -- ^ The list containing all leaves of the given item.
    composite_leaves = cl []
        where cl tl x | null ch = x : tl
                      | otherwise = foldl cl tl ch
                  where ch = composite_children x

    -- | Define a label to be used for printing tasks for the given element.
    composite_print_item :: a -- ^ The given element to generate a label for.
        -> String -- ^ The resulting label that is a textual representation for the given element.
    composite_print_item = const ""

    -- | Print the composite structure as a tree using ASCII art. The representation is considered to be both easy-readable
    -- and compact at the same time. It uses the composite_print_item method to add labels  on the right of the nodes.
    composite_treeprint :: a -- ^ The given root to generate an ASCII art tree from.
        -> String -- ^ An ASCII art representing the given element and its descendants as an ASCII art tree.
    composite_treeprint = trsh [] composite_print_item
        where trsh :: (Composite a) => [Bool] -> (a -> String) -> a -> String
              trsh b sh vl | composite_has_children vl = titl ++ precalateMap trsht "\n" chi ++ '\n' : chl
                           | otherwise = titl
                  where chs = composite_children vl
                        chi = init chs
                        chl = trsh (False:b) sh $ last chs
                        simp True = "|-o"
                        simp False = "`-o"
                        cplx True = "| "
                        cplx False = "  "
                        prec = concatMap cplx . reverse
                        pres (x:xs) = prec xs ++ simp x
                        pres [] = "o"
                        titl = pres b ++ ' ' : sh vl
                        trsht = trsh (True:b) sh


-- | A typeclass that secifies that there is a "truthiness" atached to the
-- elements of a.
class Truthfull a where
    -- | Obtain the truthiness of the object.
    truthiness :: a -- ^ The object to get the truthiness from.
               -> Bool -- ^ The truthiness of the object.

-- | A pure fabrication type used to calculate the shortest path using Dijkstra's algorithm.
data DyTu a s = (Eq a, Eq s,Num a,Ord a,Hashable s) => DyTu {dycost :: a, dyst :: s, _dyhist :: [s]}

-- | Two Dijkstra tuples are equal in case both their cost and state is equal.
instance (Eq a, Eq s) => Eq (DyTu a s) where
    (==) x y = dycost x == dycost y && dyst x == dyst y

-- | We compare Dijkstra tuples based upon the accumulated cost  thus far.
instance (Eq a, Eq s, Ord a) => Ord (DyTu a s) where
    compare = compare `on` dycost

-- |Calculates the shortest path from the given first state to the given second state using a generic function that emits
-- all transitions from that state. The result is a list of states that describe the shortest path (start and end included). If no path can be found and the search is exhausted, an empty list is returned.
dijkstra :: (Eq a,Num a,Ord a,Eq s,Hashable s) => (s -> [(a,s)]) -- ^ The transition function that takes as input a state and emits a list of states together with the cost to reach them from the given state.
    -> s -- ^ The start, from which search begins
    -> s -- ^ The end, the target we wish to reach as soon as possible
    -> [s] -- ^ A list of states (begin and end included) that describe the shortest path, if no path can be found, the empty list.
dijkstra fun start stop = dkstr HS.empty (He.insert (DyTu 0 start [start]) (He.empty :: He.MinHeap (DyTu a s)))
    where dkstr ht hp | He.isEmpty hp = []
                      | HS.member sx ht = dkstr ht hp1
                      | sx == stop = reverse hx
                      | otherwise = dkstr ht1 (foldl (flip He.insert) hp $ map (\(cy,sy) -> DyTu (cx+cy) sy (sy:hx)) $ filter (not . flip HS.member ht1 . snd) $ fun sx)
              where Just (DyTu cx sx hx,hp1) = He.view hp
                    ht1 = HS.insert sx ht

-- | An ordering function that takes two items, and returns how these are
-- ordered.
type Orf a = a -> a -> Ordering

-- | A datastructure to store two dimensional textual data where indices indicate splits between relevan pieces of data. Tables are as intermediate datastructures for printing and parsing.
data Table = Table { tbldat :: [[String]] -- ^ The 2d list storing all the two dimensional data in textual form.
                   , hlines :: [Int] -- ^ The horizonal lines that are shown/parsed.
                   , vlines :: [Int]
                   }

-- | A class that specifies that the content of the element can be printed as a table
class Tabulatable a where
    -- | Represent the data as a table.
    toTable :: a -- ^ The given element to transform to a table.
        -> Table -- ^ The table that represents the given element.
    -- | Parse the table representation to a data element.
    fromTable :: Table -- ^ The given table to parse.
        -> a -- ^ The resulting data element.

-- | A function that aims to merge a list item with the next one (and inductively further). In case the merge succeeds
-- (the merge function results a `Just _`), sticking is done further with the product and the next list item.
-- Otherwise the first item in the merge is emitted and sticking is done with the next item and the remainder of the list.
stick :: (a -> a -> Maybe a) -- ^ The given merge function. If the two items can be merged, it returns a `Just y` with `y` the result. If the items cannot be merged, `Nothing` should be returned.
    -> [a] -- ^ The given list to run the sticking algorithm on.
    -> [a] -- ^ The resulting list of stick objects.
stick f = st
    where st (x:xe@(x':xs)) | Just y <- fx = st (y:xs)
                            | otherwise = x : st xe
              where fx = f x x'
          st r = r

-- | Keep calling the given function iteratively over the given input, until the value after the function application is
-- equal to the original value. In that case the initial value is returned.
fixpoint :: (Eq a) => (a -> a) -- ^ The given function to be called repeatedly.
    -> a -- ^ The given initial value.
    -> a -- ^ The value when a fixed point is reached.
fixpoint f = fp
    where fp x | x == y = x
               | otherwise = fp y
              where y = f x

-- | Keep calling the given function iteratively over the given input, until the value after the function application is
-- equal to the original value. Emit all values up to the value that produces the same result.
scanFixpoint :: (Eq a) => (a -> a) -- ^ The given function to be called repeatedly.
    -> a -- ^ The given initial value.
    -> [a] -- ^ The list of values that is generated after each function application.
scanFixpoint f = fp
    where fp x | x == y = [x]
               | otherwise = x : fp y
              where y = f x

-- | Unroll a given list of values by updating the accumulator each time and
-- returning the emission after each transition.
unroll :: (a -> b -> (a,c)) -- ^ The update function that takes the state, input and produces output and the next state.
       -> a -- ^ The initial state.
       -> [b] -- ^ The list of inputs.
       -> [c] -- ^ The list of outputs.
unroll f = go
    where go _ [] = []
          go s (x:xs) = e : go s' xs
              where (s',e) = f s x

-- | A function that is the opposite of the intersperse function: the given value is not only emitted in between the list
-- of values, but at the outer sides as well.
outersperse :: a -- ^ The element that will be emited in between elements as well as the outer bounds.
    -> [a] -- ^ The list of elements that will be emited with the given value in between and at the outer bounds.
    -> [a] -- ^ The result of the outersperse: the given list with the given value at the bounds and in between.
outersperse x (y:ys) = x : y : outersperse x ys
outersperse x [] = [x]

-- | A variant of the intercalate function where each element in the second given list is prefixed by all elements in the first given list.
precalate :: [a] -- ^ The first given list that acts as a prefix for all elements in the second given list.
    -> [[a]] -- ^ The second given list of elements that all must be prefixed by the first given list.
    -> [a] -- ^ The resulting list that is a concatenation of the second given list where each element is prefixed with the first given list.
precalate pfx = pc
    where pc (x:xs) = pfx ++ x ++ pc xs
          pc [] = []

-- | A variant of the `precalate` function where the second list is not completely "materialized", but generated by a given generator function.
precalateMap :: (a -> [b]) -- ^ The given generator function.
     -> [b] -- ^ The given first list that acts as a prefix for all elements generated by the second given list and the given generator function.
     -> [a] -- ^ The second given list that is the seed for the given generator function.
     -> [b] -- ^ The resulting list that is the concatenation of the elements generated by the generator each prefixed by the given prefix.
precalateMap f d = precalate d . map f

-- | Generate a matrix (list of lists) with a given width and height and a generator function that determines the elements.
matrix :: (Int -> Int -> a) -- ^ The given generator function: a function that generates a value for a given row and column.
    -> Int -- ^ The given number of rows for the matrix that is generated.
    -> Int -- ^ The given number of columns for the matrix that is generated.
    -> [[a]] -- ^ The resulting matrix with the given number of rows and columns, and with values as generated with the generator function.
matrix f m n = [ [ f j i | i <- [1..n]] | j <- [1.. m]]

-- | Generate an identity matrix with a given value for ones and a given value for zeros.
identityMatrix :: a -- ^ The given value that must be filled in for elements located on the diagonal.
    -> a -- ^ The given value that must be filled in for elements that are not located on the diagonal.
    -> Int -- ^ The given number of rows for the matrix that is generated.
    -> Int -- ^ The given number of columns for the matrix that is generated.
    -> [[a]] -- ^ The resulting matrix with the given number of rows and columns that is shaped as an identity matrix.
identityMatrix v1 v0 = matrix f
    where f a b | a /= b = v0
                | otherwise = v1

type Trns a b c = a -> b -> c -> c
type Mrgr3 a = a -> a -> a -> a
type Lsdf a = Int -> a

-- | Calculate the Levenshtein distance between the two given lists of objects where one can do an equality check on.
-- The Levenshtein distance is the total amount of additions, deletions and modifications necessary to turn the first
-- list of objects into the other list.
levenshtein :: Eq a => [a] -- ^ The first given list to calculate the Levenshtein distance from.
    -> [a] -- ^ The second given list to calculate the Levenshtein distance from.
    -> Int -- ^ The total number of additions, deletions and modifications.
levenshtein = genericLevenshtein mgr tlff f f id id
    where mgr x y = min (min x y)
          tlff x y c | x == y = c
                     | otherwise = c+1
          f _ _ = succ
          genericLevenshtein :: Mrgr3 c -> Trns a b c -> Trns a b c -> Trns a b c -> Lsdf c -> Lsdf c -> [a] -> [b] -> c
          genericLevenshtein m tlf tf lf ia ib as bs = genericLevenshteinH m tlf tf lf as bs (map ib [1..]) ia 0

          genericLevenshteinH :: Mrgr3 c -> Trns a b c -> Trns a b c -> Trns a b c -> [a] -> [b] -> [c] -> Lsdf c -> Int -> c
          genericLevenshteinH _ _ _ _ [] _ cl _ _ = last cl
          genericLevenshteinH m tlf tf lf (a:as) bs cl ri rii = genericLevenshteinH m tlf tf lf as bs cn ri (rii+1)
              where cn = genericLevenshteinV m tlf tf lf a bs (ri rii:cl) ((ri.succ) rii)

          genericLevenshteinV :: Mrgr3 c -> Trns a b c -> Trns a b c -> Trns a b c -> a -> [b] -> [c] -> c -> [c]
          genericLevenshteinV _ _ _ _ _ [] _ _ = []
          genericLevenshteinV m tlf tf lf a (b:bs) (ctl:ct:cs) cl = r : genericLevenshteinV m tlf tf lf a bs (ct:cs) r
              where r = m (tlf a b ctl) (tf a b ct) (tlf a b cl)
          genericLevenshteinV _ _ _ _ _ _ ls _ = ls

-- | Calculate the similarity between the two given lists. The similarity depends on the Levenshtein distance as well as the length of the two lists. The similarity is a fractional type.
similarity :: (Eq a,Fractional b) => [a] -- ^ The first given list to calculate the similarity from.
    -> [a] -- ^ The second given list to calculate the similarity from.
    -> b -- ^ The similarity of the two given lists.
similarity x y = on (/) fromIntegral (levenshtein x y) $ lx * ly
    where lx = length x
          ly = length y

-- | Calculate the element in the given list such that the given function returns the smallest value (according to the default ordering). If there are multiple elements where the function generates the minimum value, the first of these elements is picked.
argmin :: Ord b => (a -> b) -- ^ The given evaluation function to compare elements with.
    -> [a] -- ^ The given list of elements to compare.
    -> a -- ^ Returns the element such that the given function generates the smallest value.
argmin f (y:ys) = argmin' ys (f y) y
    where argmin' (x:xs) o xo | ox < o = argmin' xs ox x
                              | otherwise = argmin' xs o xo
                              where ox = f x
          argmin' [] _ x = x
argmin _ [] = error "To determine the argmin of a list, the list should contain at least one element."

-- | Calculate the element in the given list such that the given function returns the greatest value (according to the default ordering). If there are multiple elements where the function generates the maximum value, the first of these elements is picked.
argmax :: Ord b => (a -> b) -- ^ The given evaluation function to compare elements with.
    -> [a] -- ^ The given list of elements to compare.
    -> a -- ^ Returns the element such that the given function generates the greatest value.
argmax f (y:ys) = argmax' ys oy y
    where oy = f y
          argmax' (x:xs) o xo | ox < o = argmax' xs ox x
                              | otherwise = argmax' xs o xo
                              where ox = f x
          argmax' [] _ x = x
argmax _ [] = error "To determine the argmax of a list, the list should contain at least one element."

-- | A generalization of the `argmin` function where the ordering of the evaluated elements is given as well.
argminBy :: (b -> b -> Ordering) -- ^ The given ordering function.
    -> (a -> b) -- ^ The given evaluation function to compare elements with.
    -> [a] -- ^ The given list of elements to compare.
    -> a -- ^ Returns the element such that the given function generates the smallest value.
argminBy cmp f (y:ys) = argmin' (f y) y ys
    where argmin' fo o (x:xs) | LT <- cmp fx fo = argmin' fx x xs
                              | otherwise = argmin' fo o xs
                              where fx = f x
          argmin'  _ x [] = x
argminBy _   _ [] = error "To determine the argmin of a list, the list should contain at least one element."

-- | A generalization of the `argmax` function where the ordering of the evaluated elements is given as well.
argmaxBy :: (b -> b -> Ordering) -- ^ The given ordering function.
    -> (a -> b) -- ^ The given evaluation function to compare elements with.
    -> [a] -- ^ The given list of elements to compare.
    -> a -- ^ Returns the element such that the given function generates the greatest value.
argmaxBy cmp f (y:ys) = argmax' (f y) y ys
    where argmax' fo o (x:xs) | cmp fx fo == GT = argmax' fx x xs
                              | otherwise = argmax' fo o xs
                              where fx = f x
          argmax'  _ x [] = x
argmaxBy _   _ [] = error "To determine the argmax of a list, the list should contain at least one element."

-- | Merge the two lists of string such that the strings are elementwise concatenated, but with enough spaces in between such that the columns have the same offset, similar to `paste` in Linux.
side :: Int -- ^ The number of spaces in between the two columns.
    -> [String] -- ^ The given list of elements for the left column.
    -> [String] -- ^ The given list of elements for the right column.
    -> [String] -- ^ The resulting list of rows that are concatenations of the given lists for the two columns.
side = genericSide ' '

genericSide :: a -> Int -> [[a]] -> [[a]] -> [[a]]
genericSide s n cl = genericSide' s n w cl
    where w = maximum $ map length cl

-- | Append every two equally placed strings in the given list table-wise: add spaces in between such that the strings
-- from the second table are located at the same column.
(<&|>) :: [String] -- ^ The given list of strings that are the rows for the first column.
     -> [String] -- ^ The given list of strings that are the rows for the second column.
     -> [String] -- ^ The resulting list of rows such that the two given columns are located correctly.
(<&|>) = side 0

-- | Append the second list of rows to the first list of rows (this is actually completely equivalent to a concat `(++)`).
(<&->) :: [a] -- ^ The first given list of rows.
    -> [a] -- ^ The second given list of rows.
    -> [a] -- ^ The resulting list of rows, which is a concatenation of the given lists.
(<&->) = (++)

genericSide' :: a -> Int -> Int -> [[a]] -> [[a]] -> [[a]]
genericSide' _ _ _ ls [] = ls
genericSide' s n w [] (r:rs) = (replicate (w+n) s++r) : genericSide' s n w [] rs
genericSide' s n w (l:ls) (r:rs) = (l++replicate (w+n-nl) s++r) : genericSide' s n w ls rs
    where nl = length l

sideBar :: [String] -> [String] -> [String]
sideBar lf = zipWith (\l r -> l++replicate (nl-length l) ' '++" | "++r) lf
    where nl = maximum $ map length lf

tableBar :: [[String]] -> [String]
tableBar = foldr1 sideBar

-- | Outline the given list of 2d elements onto a 2d board such that the tiles do not exceed the width constraint.
tile :: a -- ^ The given element with which empty spaces is filled.
    -> Int -- ^ The number of spaces to put between two elements horizontally.
    -> Int -- ^ The number of spaces to put between two elements vertically.
    -> Maybe Int -- ^ The optional width constraint. If Nothing, there is no width constraint.
    -> [[[a]]] -- ^ The list of 2d elements to be tiled.
    -> [[a]] -- ^ The resulting tile.
tile c0 dw dh (Just w) = tilel 0 []
    where tilel _ xs [] = xs
          tilel x xs (it:its) | null xs = tilel nw it its
                              | nw <= w = tilel nw (genericSide c0 dw xs it) its
                              | otherwise = xs ++ replicate dh [c0] ++ tilel wit it its
              where wit = maximum $ map length it
                    nw | x > 0 = x+dw+wit
                       | otherwise = wit
tile c0 dw _ Nothing = foldr1 (genericSide c0 dw)

singleGroupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
singleGroupBy f l = map (\x -> filter ((==) x . f) l) fl
    where fl = nub $ map f l

-- | For a given list generate a list of tuples such that it contains every two consecutive elements of the original list.
steps :: [a] -- ^ The given list.
    -> [(a,a)] -- ^ The resulting list containing every two consecutive elements.
steps (x:xs@(x2:_)) = (x,x2) : steps xs
steps _ = []

-- | For a given list generate a list of tuples such that it contains every two consecutive elements of the original list. The first tuple is however the first element repeated twice.
steps0 :: [a] -- ^ The given list.
    -> [(a,a)] -- ^ The resulting list containing every two consecutive elements the first element is stored in the first tuple twice.
steps0 xl@(x:_) = (x,x) : steps xl
steps0 _ = []

-- | For a given list map every two consecutive items to a new item and emit these items.
mapSteps :: (a -> a -> b) -- ^ The given map function to be called.
     -> [a] -- ^ The given list of values.
     -> [b] -- ^ The resulting list of elements that is a mapping from every two consecutive elements of the original list.
mapSteps f = ms
   where ms (x:xs@(x2:_)) = f x x2 : ms xs
         ms _ = []

-- Determine that for every two consecutive elements in the list satisfy the given predicate.
allSteps :: (a -> a -> Bool) -- ^ The given predicate to validate.
    -> [a] -- ^ The given predicate to verify.
    -> Bool -- ^ A boolean that is `True` if all two consecutive elements satisfy the given predicate; False otherwise.
allSteps f = all (uncurry f) . steps

-- | Given an item, generate a list with that item, the list thus contains one element.
singleton :: a -- ^ The given element to be put in the list.
    -> [a] -- ^ The resulting list of length one.
singleton x = [x]

specifyMerge :: (a -> a -> Maybe a) -> [a] -> [a]
specifyMerge _ [] = []
specifyMerge f (l:ls) = specifyMerge' ls 1 l
    where specifyMerge' [] n x = replicate n x
          specifyMerge' (x2:xs) n x | Just y <- f x x2 = specifyMerge' xs (n+1) y
                                    | otherwise = replicate n x++specifyMerge' xs 1 x2

-- | Generate a list of tuples each containing an element of the original list together with the result of the element applied to the given function.
tupleMap :: (a -> b) -- ^ The given function to call.
    -> [a] -- ^ The list of elements to process.
    -> [(a,b)] -- ^ The resulting list of tuples containing the original element together with the function application of that element.
tupleMap = map . withf

-- | Link all the elements in the given list to the list in the given hashmap.
hashItemList :: (Eq a,Hashable a) => [a] -- ^ The given list of elements to add to the hashmap
    -> HM.HashMap a [a] -- ^ The hashmap where elements should be added to.
    -> HM.HashMap a [a] -- ^ The resulting hashmap where every element to the list is linked to the list.
hashItemList li hi = foldl (\hm x -> HM.insert x li hm) hi li

hashItemLists :: (Eq a,Hashable a) => [[a]] -> HM.HashMap a [a] -> HM.HashMap a [a]
hashItemLists ls hm = foldl (flip hashItemList) hm ls

hashItemIndex :: (Eq a,Hashable a,Integral i) => i -> [a] -> HM.HashMap a i -> HM.HashMap a i
hashItemIndex i0 li hi = foldl (\hm (x,y) -> HM.insert x y hm) hi $ zip li [i0..]

hashGenerator :: (Eq a,Hashable a,Integral i) => (i -> a -> b) -> i -> [a] -> HM.HashMap a b -> HM.HashMap a b
hashGenerator f i0 li hi = foldl (\hm (x,y) -> HM.insert x (f y x) hm) hi $ zip li [i0..]

-- | Call the given function, in case it returns a `Just _`, the result is returned, otherwise the original value is returned.
maybeId :: (a -> Maybe a) -- ^ The given function to call.
    -> a -- ^ The given element.
    -> a -- ^ The resulting element. In case the given function returns a `Just _`, the result is returned, otherwise the original value is returned.
maybeId = ap fromMaybe

-- | Turn a function into a variant where the given parameters are `Maybe` as well. In case one of the arguments is `Nothing`, ``Nothing` is returned; otherwise the result of the function application is returned.
maybenize :: Monad m => (a -> b -> m c) -- ^ The given function to transform.
    -> m a -- ^ The first argument to call. If `Nothing`, `Nothing` will be returned.
    -> m b -- ^ The second argument to call. If `Nothing`, `Nothing` will be returned.
    -> m c -- ^ The result of the function application.
maybenize f ma mb = ma >>= \a -> mb >>= f a

varRg :: (a -> b) -> a -> c -> b
varRg = (const .)

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) g f x = g . f x

-- | A variant of the `tail` function. This function is however absolute: if the empty list is given, the empty list is returned.
safeTail :: [a] -- ^ The given list to calculate a safe tail from.
    -> [a] -- ^ The resulting tail of the list.
safeTail (_:xs) = xs
safeTail x = x

-- | A variant of the `init` function. This function is however absolute: if the empty list is given, the empty list is returned.
safeInit :: [a] -- ^ The given list to calculate a safe init from.
    -> [a] -- ^ The resulting init of the list.
safeInit (x:xs@(_:_)) = x : safeInit xs
safeInit _ = []

-- | Generate a list of heads from the lists of the the given list. Empty lists -- that do not have a head -- will not emit a head to the list. The length of the resulting list can thus be smaller than the length of the original list.
safeMapHeads :: [[a]] -- ^ The given list to calculate the heads from.
    -> [a] -- ^ The resulting list containing all the heads of the lists in the given list.
safeMapHeads [] = []
safeMapHeads ([]:ys) = safeMapHeads ys
safeMapHeads ((x:_):ys) = x : safeMapHeads ys

-- | Tries to find the first element in the given list that matches the given predicate. If no such element is found, it returns the given default.
findDefault :: a -- ^ The given default to return if the element is not found.
    -> (a -> Bool) -- ^ The given predicate to validate elements in the given list.
    -> [a] -- ^ The given list.
    -> a -- ^ The first element in the given list that matches the given predicate. If no such element is found, the given default.
findDefault dflt f = fromMaybe dflt . find f

-- | A successor rotate function. A modified sucessor function that, when the maximum bound is reached, returns the minimum bound.
succR :: (Bounded a,Enum a, Eq a) => a -- ^ The given element to perform successor rotate on.
    -> a -- The successor of the element, or the `minBound` if the given element was the `maxBound`
succR x | x /= maxBound = succ x
        | otherwise = minBound

-- | A predecessor rotate function. A modified predecessor function that, when the minimum bound is reached, returns the maximum bound.
predR :: (Bounded a,Enum a, Eq a) => a -- ^ The given element to perform predecessor rotate on.
    -> a -- The predecessor of the element, or the `maxBound` if the given element was the `minBound`
predR x | x /= minBound = pred x
        | otherwise = maxBound

-- | Repeat the given function a given amount of times on the given input.
nF :: Int -- ^ The given number of times the given function should be repeated.
    -> (a -> a) -- ^ The given function to call a given amount of times.
    -> a -- ^ The given value where the given function is repeatedly being called on.
    -> a -- ^ The result after calling the given function a given amount of times.
nF n f x | n > 0 = nF (n-1) f $ f x
         | otherwise = x

-- | Call the successor rotate function a given amount of times.
nSuccR :: (Bounded a,Enum a, Eq a) => Int -- ^ The number of times the successor rotate function should be called.
    -> a -- ^ The element on which the successor rotate function is called a given amount of times.
    -> a -- ^ The resulting value.
nSuccR n = nF n succR

-- | Call the predecessor rotate function a given amount of times.
nPredR :: (Bounded a,Enum a, Eq a) => Int -- ^ The number of times the predecessor rotate function should be called.
    -> a -- ^ The element on which the successor rotate function is called a given amount of times.
    -> a -- ^ The resulting value.
nPredR n = nF n predR

-- | A function that ensures that the given value is between the given minimum and maximum according to the given order function. If not, the appropriate bound is returned. The lower bound should be less than or equal to the upper bound.
genericBorder :: Orf a -- ^ The given order function.
    -> a -- ^ The given lower bound that should be respected.
    -> a -- ^ The given upper bound that should be respected.
    -> a -- ^ The given value.
    -> a -- ^ The value if between the bounds, the closest bound otherwise.
genericBorder orf mn mx val | LT <- cmp mn = mn
                            | GT <- cmp mx = mx
                            | otherwise = val
    where cmp = orf val

-- | A function that ensures that the given value is between the given minimum and maximum. If not, the appropriate bound is returned. The lower bound should be less than or equal to the upper bound.
border :: Ord a => a -- ^ The given lower bound that should be respected.
    -> a -- ^ The given uper bound that should be respected.
    -> a -- ^ The given value.
    -> a -- ^ The value if between the bounds, the closest bound otherwise.
border = genericBorder compare

-- | Set the first element of the tuple to the given new value.
setFst :: c -- ^ The given new value for the first element of the given tuple.
    -> (a,b) -- ^ The given tuple to be modified.
    -> (c,b) -- ^ The resulting tuple that is the given tuple but with the first element modified.
setFst = first . const

-- | Set the second element of the tuple to the given new value.
setSnd :: c -- ^ The given new value for the second element of the given tuple.
    -> (a,b) -- ^ The given tuple to be modified.
    -> (a,c) -- ^ The resulting tuple that is the given tuple but with the second element modified.
setSnd = second . const

-- | Apply the given expression on the given list of arguments, used to produce template Haskell products.
appEs :: Exp -- ^ The given function to be aplied on the given list of arguments.
    -> [Exp] -- ^ The given list of arguments that are aplied on the given function.
    -> Exp -- ^ The resulting expression that is the given function aplied on the given list of arguments.
appEs = foldl AppE

-- | Run the given function a given amount of times on the given input element.
fTimes :: (Num n,Ord n) => (a -> a) -- ^ The given function to use for the iterations.
    -> n -- ^ The given amount of time the iteration should be performed. If less than zero, the function is applied zero times.
    -> a -- ^ The given initial value to iterate over.
    -> a -- ^ The result after applying the given function the given amount of times.
fTimes f = ft'
    where ft' n | n > 0 = ft' (n-1) . f
                | otherwise = id

-- | Run the given function a given amount of times on the given input element. But stop from the moment the product is equal to the given value. This can be faster than its brother `fTimes` if it is likely to reach a *fixed point*.
fTimesEq :: (Num n,Ord n,Eq a) => (a -> a) -- ^ The given function to use for the iterations.
    -> n -- ^ The given amount of time the iteration should be performed. If less than zero, the function is applied zero times.
    -> a -- ^ The given initial value to iterate over.
    -> a -- ^ The result after applying the given function the given amount of times.
fTimesEq f = ft'
    where ft' n x | n <= 0 = x
                  | x == fx = x
                  | otherwise = ft' (n-1) fx
              where fx = f x

-- | Run the given reduce function over the given mapping of the given list of elements.
mapred :: ([b] -> c) -- ^ The given reduce function.
    -> (a -> b) -- ^ The given map function.
    -> [a] -- ^ The given list of elements to mapreduce.
    -> c -- ^ The result of reducing the mapped elements.
mapred mgr f = mgr . map f

-- | A template Haskell function where for a given number, a function is generated that takes as input a function "f" and a tuple with *n* elements. The function is then applied to all elements of the given tuple.
fTup :: Int -- ^ The given number of tuple elements in the generated function.
    -> Q Exp -- ^ The resulting generated function.
fTup n = do
    fs <- replicateM n $ newName "f"
    xs <- replicateM n $ newName "x"
    return $ LamE (map VarP fs++[TupP (map VarP xs)]) $ TupE $ zipWith (AppE `on` VarE) fs xs

overMN :: Int -> Int -> Q Exp
overMN m n = do
    f <- newName "f"
    gs <- replicateM m $ newName "g"
    xs <- replicateM n $ newName "x"
    return $ LamE (VarP f:map VarP gs++map VarP xs) $ appEs (VarE f) $ map (\g -> appEs (VarE g) (map VarE xs)) gs

-- | A template Haskell function that for a given number generates a `map` function that is nested *n* deep with *n* the given number.
mapN :: Int -- ^ The given number *n*.
    -> Q Exp -- ^ The resulting generated function.
mapN n = do
    f <- newName "f"
    return $ LamE [VarP f] $ foldr AppE (VarE f) $ replicate n $ VarE $ Name (OccName "map") $ NameQ $ ModName "Data.List"

-- | A template Haskell function that for a given number generates a `const` function that takes the given amount of parameters (and does nothing with them).
constN :: Int -- ^ The given number of parameters to add to the function.
    -> Q Exp -- ^ The resulting generated function.
constN n = return $ fTimes (AppE appf) (n-1) constf
    where dotf = VarE $ Name (OccName ".") $ NameQ $ ModName "Data.Function"
          constf = VarE $ Name (OccName "const") $ NameQ $ ModName "Data.Function"
          appf = AppE dotf constf

-- | A template Haskell function that generates for a given *n* and *k* a function that takes as input a tuple with *n* items and returns the *k*-th element. *k* has offset `0`, thus `$(selN 3 2)` will return the last element of a tuple of three.
selN :: Int -> Int -> Q Exp
selN n k | n > 0 && k >= 0 && k < n = do
    x <- newName "x"
    return $ LamE [TupP $ replicate k WildP++VarP x : replicate (n-k-1) WildP] $ VarE x
         | n <= 0 = error "The number of elements of the tuple must be at least one."
         | otherwise = error "K must be between 0 (inclusive) and n (exclusive)."

-- | A template Haskell function that for a given number *n* generates a function that takes as input a function that takes as input *n* values as well as a tuple with *n* items and calls the function with the values in the tuple.
uncurryN :: Int -- ^ The given number *n*.
     -> Q Exp -- ^ The resulting generated function.
uncurryN n = do
    f <- newName "f"
    xs <- replicateM n $ newName "x"
    return $ LamE [VarP f,TupP $ map VarP xs] $ appEs (VarE f) $ map VarE xs

-- | A template Haskell function that for a given number *n* generates a function that takes as input a function that takes as input a tuple with *n* elements as well as *n* values. The *n* values are then packed together in a tuple and the given function is called with that tuple.
curryN :: Int -- ^ The given number *n*.
     -> Q Exp -- ^ The resulting generated function.
curryN n = do
    f <- newName "f"
    xs <- replicateM n $ newName "x"
    return $ LamE (VarP f:map VarP xs) $ AppE (VarE f) $ TupE $ map VarE xs

groupF :: Int -> Q Exp
groupF n = do
    fs <- replicateM n $ newName "f"
    x <- newName "x"
    return $ LamE (map VarP fs++[VarP x]) $ TupE $ map (\f -> AppE (VarE f) (VarE x)) fs

funTup :: Int -> Int -> Q Exp
funTup m n = do
    f <- newName "f"
    xss <- replicateM m (replicateM n $ newName "x")
    return $ LamE (VarP f : map (TupP . map VarP) xss) $ TupE $ map (appEs (VarE f) . map VarE) $ transpose xss

-- | Select, based on the given boolean value whether the first or the second element is returned.
selectBool :: Bool -- ^ The given boolean that determines which value is returned.
     -> a -- ^ The given value that is returned if the given boolean was true.
     -> a -- ^ The given value that is returned if the given boolean was false.
     -> a -- ^ The resulting value that is one of the two above, depending on the value of the given boolean.
selectBool True x = const x
selectBool _    _ = id

-- | Select, based on the given number whether the first or the second element is returned.
selectNum :: (Num a,Eq a) => a -- ^ The given number that determines which value is returned.
    -> b -- ^ The given value that is returned if the given number is *not* zero.
    -> b -- ^ The given value that is returned if the given number is zero.
    -> b -- ^ The resulting value that is one of the two above, depending on the value of the given number.
selectNum 0 _ = id
selectNum _ x = const x

-- | Check whether the given list of numbers is a distribution: a distribution is a list of values such that every value
-- greater than or equal to zero and the distribution sums up to a given number (one, hundred, or an arbitrary positive).
distribution :: (Num a,Ord a) => a -- ^ The given value to which the distribution should sum up.
    -> [a] -- ^ The given distribution that is checked.
    -> Bool -- ^ The returning value determining whether the given list is a distribution. True if it is; False otherwise.
distribution t = f 0
    where f a (x:xs) = x >= 0 && x <= t && f (x+a) xs
          f a _      = t == a

-- | A specialization of the `distribution` function that determines that the distribution should sum up to one hundred.
distribution100 :: (Num a,Ord a) => [a] -- ^ The given distribution that is checked.
     -> Bool -- ^ The returning value determining whether the given list is a distribution. True if it is; False otherwise.
distribution100 = distribution 100

replicateFoldl1 :: (a -> a -> a) -> Int -> a -> a
replicateFoldl1 f n = foldl1 f . replicate n

genericReplicateItems ::  a -> (Int -> [a]) -> Int -> [(Int,Int)] -> [a]
genericReplicateItems c0 c1 n = ri' 0
    where ri' i ((a,da):as) = replicate (max 0 $ min (n-i) $ a-i) c0 ++ c1 (max 0 $ min (n-a) da) ++ ri' (a+da) as
          ri' i []          = replicate (max 0 $ n-i) c0

replicateItems :: a -> a -> Int -> [(Int,Int)] -> [a]
replicateItems c0 c1 = genericReplicateItems c0 (`replicate` c1)

burstItems :: a -> a -> a -> a -> Int -> [(Int,Int)] -> [a]
burstItems c0 c1 c2 c3 = genericReplicateItems c0 $ burst c1 c2 c3

concatReplicate :: Int -> [a] -> [a]
concatReplicate a = concat . replicate a

-- Generate a list that starts with the head item followed by n-2 times the
-- middle item followed by the tail item.
burst :: a
      -> a
      -> a
      -> Int
      -> [a]
burst a b c n = a : replicate (n-2) b ++ [c]

burstInner :: a -> a -> a -> [a] -> Int -> [a]
burstInner a b c i n = a : replicate n0 b ++ i ++ replicate n1 b ++ [c]
    where ni = length i
          n0 = div (n-2-ni) 2
          n1 = n-2-ni-n0

adjustsSq :: (a -> a) -> [Int] -> Sq.Seq a -> Sq.Seq a
adjustsSq f = flip $ foldr $ Sq.adjust f

commutative :: b -> (a -> a -> Maybe b) -> a -> a -> b
commutative v f x y | Just d <- f x y = d
                    | Just d <- f y x = d
                    | otherwise = v

withf :: (a -> b) -> a -> (a,b)
withf = ap (,)

mapWithf :: (a -> b) -> [a] -> [(a,b)]
mapWithf f = map (withf f)

-- | Generate a list containing unique elements by a given equality criterion, but with the precondition that the list is sorted. The ordering must be complete in the sense that if `compare x y == EQ`, then `x == y`.
ordNubBy :: (a -> a -> Bool) -- ^ The given equality test function.
    -> [a] -- ^ The given list of elements that must be processed through the uniqueness filter, must be ordered.
    -> [a] -- ^ The resulting list containing only unique (and sorted) elements of the given list.
ordNubBy eq (l:ls) = f' l ls
    where f' x (y:ys) | eq x y = f' x ys
                      | otherwise = x : f' y ys
          f' x [] = [x]
ordNubBy _ [] = []

-- | Generate a list containing unique elements by the default equality function, but with the precondition that the list is sorted. The ordering must be complete in the sense that if `compare x y == EQ`, then `x == y`.
ordNub :: Eq a => [a] -- ^ The given list of elements that must be processed through the uniqueness filter, must be ordered.
    -> [a] -- ^ The resulting list containing only unique (and sorted) elements of the given list.
ordNub = ordNubBy (==)

genericFastSortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
genericFastSortByOn cmp f = map fst . sortBy (on cmp snd) . tupleMap f

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (on compare f)

fastSortByOn :: Ord b => (a -> b) -> [a] -> [a]
fastSortByOn = genericFastSortByOn compare

-- | Merge the two ordered lists into a new ordered list by a given comparison function.
mergeOrdBy :: (a -> a -> Ordering) -- ^ The given order function that is respected by the two given lists to be merged.
    -> [a] -- ^ The first given list to be merged, must be sorted.
    -> [a] -- ^ The second given list to be merged, must be sorted.
    -> [a] -- ^ The resulting list containing all elements of the two given lists (duplicates included) that is ordered as well.
mergeOrdBy cmp = mob
    where mob xl@(x:xs) yl@(y:ys) | cmp x y == GT = y : mob xl ys
                                  | otherwise = x : mob xs yl
          mob [] ys = ys
          mob xs [] = xs

-- | A special version of the `mergeOrdBy` function where first a mapping is done to another element that can be compared. The given lists still need to be ordered according to this mapping.
mergeOrdOn :: Ord b => (a -> b) -- ^ The given mapping function that maps elements to a comparable value.
    -> [a] -- ^ The first given list to be merged, must be sorted.
    -> [a] -- ^ The first second list to be merged, must be sorted.
    -> [a] -- ^ The resulting list containing all elements of the two given lists (duplicates included) that is ordered as well.
mergeOrdOn f = mergeOrdBy (on compare f)

-- | A special version of the `mergeOrdBy` function where the default comparison `compare` is used as order criterion.
mergeOrd :: Ord a => [a] -- ^ The first given list to be merged, must be sorted.
    -> [a] -- ^ The second given list to be merged, must be sorted.
    -> [a] -- ^ The resulting list containing all elements of the two given lists (duplicates included) that is ordered as well.
mergeOrd = mergeOrdBy compare

subOrdBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
subOrdBy cmp = sob
    where sob xl@(x:xs) yl@(y:ys) | GT <- cmpxy = sob xl ys
                                  | LT <- cmpxy = x : sob xs yl
                                  | otherwise = sob xs yl
              where cmpxy = cmp x y
          sob xs _ = xs

subOrd :: Ord a => [a] -> [a] -> [a]
subOrd = subOrdBy compare

-- | Compare first on the first given map function. If equal the comparison is determined by the second map function; otherwise the result of the first mapped ordering is used.
cmpThenOn :: (Ord b,Ord c) => (a -> b) -- ^ The first given mapping function.
    -> (a -> c) -- ^ The second given mapping function.
    -> a -- ^ The first given element to compare.
    -> a -- ^ The second element to compare.
    -> Ordering -- ^ The ordering based on the first mapping function and the second as a tie-breaker.
cmpThenOn f g x y = mappend (on compare f x y) (on compare g x y)

spread :: (Enum b,Ord b) => b -> a -> [(b,a)] -> [a]
spread i x yl@((j,y):ys) | j <= i = spread i y ys
                         | otherwise = x : spread (succ i) x yl
spread _ x [] = repeat x

spreadh :: (Enum b,Ord b) => b -> [(b,a)] -> [a]
spreadh i l@((_,h):_) = spread i h l
spreadh _ [] = []

spread0 :: (Num b,Ord b,Enum b) => a -> [(b,a)] -> [a]
spread0 = spread 0

spread0h :: (Num b,Ord b,Enum b) => [(b,a)] -> [a]
spread0h = spreadh 0

-- | Filter the ANSI escape characters out of the given string.
filterAnsi :: String -- ^ The given string to scrape the ANSI escape characters from.
    -> String -- ^ The resulting string not containing any ANSI escape characters anymore.
filterAnsi ('\x1b':xs) = fa xs
    where fa ('m':ys) = filterAnsi ys
          fa (_:ys) = fa ys
          fa [] = []
filterAnsi (x:xs) = x : filterAnsi xs
filterAnsi [] = []

-- | Display binary data as a sequence of zeros and ones separated by spaces for each eight bits.
showBinary :: FiniteBits a => a -- ^ The given value to display binary.
    -> String -- ^ The resulting string that is the binary representation of the given value with spaces to make it more readable.
showBinary x = tl n0 : sb (n0-1)
    where n0 = finiteBitSize x-1
          sb n | n < 0 = []
               | mod n 8 == 7 = ' ' : tl n : sb (n-1)
               | otherwise = tl n : sb (n-1)
          tl n | testBit x n = '1'
               | otherwise = '0'

-- Generate from the given number `x` the sequence `x`, `x+1`, `x-1`, `x+2`, `x-2`, ...
alternateSeq :: (Enum a,Num a) => a -- ^ The first number in the sequence.
    -> [a] -- ^ The resulting list of alternating numbers.
alternateSeq x = x : concat [[x+i,x-i] | i <- [1..]]

-- Generate from the given number `x` the sequence `x`, `x-1`, `x+1`, `x-2`, `x+2`, ...
negAlternateSeq :: (Enum a,Num a) => a -- ^ The first number in the sequence.
    -> [a] -- ^ The resulting list of alt e rnating numbers.
negAlternateSeq x = x : concat [[x-i,x+i] | i <- [1..]]

-- | Keep retrying to call the function upon all elements of the given list. If one of the elements `x` produces a `Just y`, `y` is returned together with its caller `x`. Otherwise the default `x0` and `y0` are returned.
retryMaybe :: (a -> Maybe b) -- ^ The given function `f` to be called repeatedly.
    -> (a,b) -- ^ The given default `x0` and `y0` value.
    -> [a] -- ^ The list of values for `x`, `f` is called with.
    -> (a,b) -- ^ The resulting tuple of `y` and its corresponding caller `x`.
retryMaybe f = foldr g
    where g x y | Just fx <- f x = (x,fx)
                | otherwise = y

-- | Strip off the spacing characters at the beginning of the string.
trimStringLeft :: String -- ^ The initial string that is to be trimmed.
    -> String -- ^ The returning string that is trimmed.
trimStringLeft = dropWhile isSpace

-- | Strip off the spacing characters at the end of the string.
trimStringRight :: String -- ^ The initial string that is to be trimmed.
    -> String -- ^ The returning string that is trimmed.
trimStringRight = dropWhileEnd isSpace

-- | Stript off the spacing characters at the beginning and the end of the string.
trimString :: String -- ^ The initial string that is to be trimmed.
    -> String -- ^ The returning string that is trimmed.
trimString = trimStringRight . trimStringLeft

-- | Ensure that all lists have the same length by either appending or removing characters from the lists.
equalWidth :: a -- ^ The given character that must be appended when the string is to short.
    -> Int -- ^ The targeted length for all lists.
    -> [[a]] -- ^ The list of lists to manipulate.
    -> [[a]] -- ^ The resulting list of lists after manipulation.
equalWidth c0 w = ew
    where ew (x:xs) | wi < w = (x++replicate (w-wi) c0) : tl
                    | wi > w = take w x : tl
                    | otherwise = x :  tl
              where wi = length x
                    tl = ew xs
          ew [] = []

-- | Perform an increment by toggling only one bit (as a Gray counter is supposed to do).
grayInc :: (Bits a,Ord a,Num a,Enum a) => Int -- ^ The given number of bits of the counter.
    -> a -- ^ The given initial value of the counter.
    -> a -- ^ The resulting value of the counter after increment.
grayInc n b | even (popCount b) = complementBit b 0
            | lst > lbm = xor b (shiftL lbm 1)
            | otherwise = 0
    where lbm = b .&. succ (complement b)
          lst = shiftL 1 $ pred n

-- | Calculate the power of a certain number given the default value for powers less than or equal to zero, the element to takea power from, the multiplication function and the power to which the number should be raised. Can be used for fast replicate element constructions.
powerl :: (Integral n,Ord n) => a -- ^ The given value to return if the power is less than or equal to zero.
    -> a  -- ^ The given element to calculate a power from.
    -> (a -> a -> a) -- ^ The given multiplication function, should be commutative and associative.
    -> n -- ^ The given power.
    -> a -- ^ Returning element which is the given element to the given power.
powerl x1 x f n0 | n0 > 0 = pf x n0
                 | otherwise = x1
    where pf x0 n | even n = sq
                  | n > 1 = f x0 sq
                  | otherwise = x0
              where sq = pf (f x0 x0) (div n 2)

nand :: Foldable f => f Bool -> Bool
nand = not . and

nor :: Foldable f => f Bool -> Bool
nor = not . or

xoring :: Foldable f => f Bool -> Bool
xoring = foldr (/=) False

xnoring :: Foldable f => f Bool -> Bool
xnoring = not . xoring
