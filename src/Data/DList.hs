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

newtype DList a = DList ([a] -> [a])

toList :: DList a -> [a]
toList (DList dl) = dl []

singleton :: a -> DList a
singleton x = DList (x:)

empty :: DList a
empty = DList id

append :: DList a -> DList a -> DList a
append (DList xs) (DList ys) = DList (xs . ys)
