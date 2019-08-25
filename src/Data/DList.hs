{-# LANGUAGE Safe #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Minimal API-compatible rip-off of @Data.DList@
module Data.DList
    ( DList
    , empty
    , singleton
    , append
    , toList
    ) where

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.
newtype DList a = DList ([a] -> [a])

-- | Convert a dlist to a list
toList :: DList a -> [a]
toList (DList dl) = dl []

-- | Create dlist with a single element
singleton :: a -> DList a
singleton x = DList (x:)

-- | Create a dlist containing no elements
empty :: DList a
empty = DList id

-- | O(1). Append dlists
append :: DList a -> DList a -> DList a
append (DList xs) (DList ys) = DList (xs . ys)
