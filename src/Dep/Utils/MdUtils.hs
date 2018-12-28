-- | A utility module for monadic actions and processing.
module Dep.Utils.MdUtils(
    orMaybeM,orBoolM
    ) where

-- | A monadic function that calls the given monadic function on the list of values until either one returns `Just x`, or the end of the list is finished. In case one of the calls returns `Just x`, `Just x` is returned; otherwise the monad returns `Nothing`.
orMaybeM :: Monad m => (a -> m (Maybe b)) -- ^ The given monadic function.
    -> [a] -- ^ The given list of elements that provide input for the given monadic function.
    -> m (Maybe b) -- ^ The resulting monad that keeps trying to call the monadic function on all elements until one element return a `Just x`.
orMaybeM f = g
    where g (x:xs) = f x >>= h
              where h (Just y) = return $ Just y
                    h _ = g xs
          g _ = return Nothing

-- | The monadic equivalent of an `or` operation with a monadic generator. From the moment that one of the Monads emits True,
-- the result is True. If all the monads return False (and there is an empty amount of them, the result is False).
orBoolM :: Monad m => (a -> m Bool) -- ^ The given monadic generator function.
    -> [a] -- ^ The given list of generator seeds.
    -> m Bool -- ^ The result: a monadic Bool that is True if any of the above generated monads returned True, False otherwise.
orBoolM f = g
    where g (x:xs) = f x >>= h
              where h True = return True
                    h _ = g xs
          g _ = return False
