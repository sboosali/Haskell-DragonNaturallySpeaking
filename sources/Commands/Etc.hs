module Commands.Etc where

-- https://hackage.haskell.org/package/bool-extras-0.4.0/docs/src/Data-Bool-Extras.html#bool
{-# INLINE bool #-}
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

