{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Internal
    ( Node(..)
    , nodeLoc
    , NodeId
    , Doc(..)
    , Mapping
    ) where

import qualified Data.Map                  as Map

import           Data.YAML.Event           (Tag)
import           Data.YAML.Loader          (NodeId)
import           Data.YAML.Schema.Internal (Scalar (..))

import           Util

-- | YAML Document tree/graph
--
-- __NOTE__: In future versions of this API meta-data about the YAML document might be included as additional fields inside 'Doc'
newtype Doc n = Doc
   { docRoot :: n -- ^ @since 0.2.1
   } deriving (Eq,Ord,Show,Generic)

-- | @since 0.2.0
instance NFData n => NFData (Doc n) where
  rnf (Doc n) = rnf n

-- | @since 0.2.1
instance Functor Doc where
  fmap f (Doc n) = Doc (f n)
  x <$ _ = Doc x

-- | YAML mapping
type Mapping loc = Map (Node loc) (Node loc)

-- | YAML Document node
--
-- @since 0.2.0
data Node loc
  = Scalar   !loc !Scalar
  | Mapping  !loc !Tag (Mapping loc)
  | Sequence !loc !Tag [Node loc]
  | Anchor   !loc !NodeId !(Node loc)
  deriving (Show,Generic)

nodeLoc :: Node loc -> loc
nodeLoc (Scalar pos _)     = pos
nodeLoc (Anchor pos _ _)   = pos
nodeLoc (Mapping pos _ _)  = pos
nodeLoc (Sequence pos _ _) = pos

instance Functor Node where
  fmap f node = case node of
    Scalar   x scalar -> Scalar   (f x) scalar
    Mapping  x tag m  -> Mapping  (f x) tag (mappingFmapLoc f m)
    Sequence x tag s  -> Sequence (f x) tag (map (fmap f) s)
    Anchor   x n nod  -> Anchor   (f x) n (fmap f nod)

mappingFmapLoc :: (a -> b) -> Mapping a -> Mapping b
mappingFmapLoc f = Map.mapKeysMonotonic (fmap f) . Map.map (fmap f)

instance Eq (Node loc) where
  Scalar   _ a    ==  Scalar   _ a'    = a == a'
  Mapping  _ a b  ==  Mapping  _ a' b' = a == a' && b == b'
  Sequence _ a b  ==  Sequence _ a' b' = a == a' && b == b'
  Anchor   _ a b  ==  Anchor   _ a' b' = a == a' && b == b'
  _ == _ = False

instance Ord (Node loc) where
  compare (Scalar _ a)      (Scalar _ a')      = compare a a'
  compare (Scalar _ _)      (Mapping _ _ _)    = LT
  compare (Scalar _ _)      (Sequence _ _ _)   = LT
  compare (Scalar _ _)      (Anchor _ _ _)     = LT

  compare (Mapping _ _ _)   (Scalar _ _)       = GT
  compare (Mapping _ a b)   (Mapping _ a' b')  = compare (a,b) (a',b')
  compare (Mapping _ _ _)   (Sequence _ _ _)   = LT
  compare (Mapping _ _ _)   (Anchor _ _ _)     = LT

  compare (Sequence _ _ _)  (Scalar _ _)       = GT
  compare (Sequence _ _ _)  (Mapping _ _ _)    = GT
  compare (Sequence _ a b)  (Sequence _ a' b') = compare (a,b) (a',b')
  compare (Sequence _ _ _)  (Anchor _ _ _)     = LT

  compare (Anchor _ _ _)    (Scalar _ _)       = GT
  compare (Anchor _ _ _)    (Mapping _ _ _)    = GT
  compare (Anchor _ _ _)    (Sequence _ _ _)   = GT
  compare (Anchor _ a b)    (Anchor _ a' b')   = compare (a,b) (a',b')
