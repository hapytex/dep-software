-- | A module that aims to provide a transparant way to edit parts of objects by reference (`Data.IORef`)
module Dep.Utils.IORefFun(Reference(..),IORefFun(),newRefFun,toRefFun,toRefFunFst,toRefFunSnd) where

import Control.Applicative((<$>))
import Data.IORef(IORef,modifyIORef,readIORef,writeIORef)

-- | An IORef but with a getter and setter such that only a part of the information is visible and can be edited.
data IORefFun a b = IORefFun { _getter :: a -> b, _setter :: b -> a -> a, _reference :: IORef a }

-- | Construct a new IORefFun object, since the data constructor is private, this is the only public constructor.
newRefFun :: (a -> b) -> (b -> a -> a) -> IORef a -> IORefFun a b
newRefFun = IORefFun

-- | Generate an IORefFun that is the complete equivalent of the given IORef.
toRefFun :: IORef a -- ^ The given IORef to convert.
    -> IORefFun a a -- ^ The resulting IORefFun that is the equivalent of the given IORef.
toRefFun = IORefFun id $ flip const

-- | Generate an IORef that gets and sets only the first part of the referenced tuple.
toRefFunFst :: IORef (a,b) -- ^ The given reference to the tuple to get elements from.
    -> IORefFun (a,b) a -- ^ The resulting IORefFun that gets and sets the first element of the referenced tuple.
toRefFunFst = IORefFun fst (\x (_,y) -> (x,y))

-- | Generate an IORef that gets and sets only the second part of the referenced tuple.
toRefFunSnd :: IORef (a,b) -- ^ The given reference to the tuple to get elements from.
    -> IORefFun (a,b) b -- ^ The resulting IORefFun that gets and sets the second element of the referenced tuple.
toRefFunSnd = IORefFun snd (\y (x,_) -> (x,y))

-- | A class that specifies `a` is referencing to (part) of some call-by-reference data structure.
class Reference ref where
    -- | Use the getter of the reference to receive the correct data.
    readReference :: ref b -- ^ The given reference to access the data from.
        -> IO b -- ^ The I/O operation together with the resulting object to get.
    -- | Use the setter of the reference to set part of the data.
    writeReference :: ref b -- ^ The reference to which (a part) must be modified.
        -> b -- ^ The new value that must be written to the scope of the reference.
        -> IO () -- ^ An I/O operation that returns nothing.
    --writeReference r v = writeReference r (const v)
    -- | Apply a function to the given scope of the reference.
    modifyReference :: ref b -- ^ The given reference to (partly) modify.
        -> (b -> b) -- ^ The given modification function.
        -> IO () -- ^ An I/O operation that returns nothing.
    --modifyReference r f = readReference >>= writeReference r . f

instance Reference IORef where
    readReference = readIORef
    writeReference = writeIORef
    modifyReference = modifyIORef

instance Reference (IORefFun a) where
    readReference r = _getter r <$> readIORef (_reference r)
    writeReference r v = readIORef refr >>= writeIORef refr . _setter r v
        where refr = _reference r
    modifyReference r f = do
        st <- readIORef refr
        writeIORef refr $ _setter r (f $ _getter r st) st
        where refr = _reference r
