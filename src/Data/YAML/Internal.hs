{-# LANGUAGE Safe #-}

module Data.YAML.Internal
    ( Node(..)
    ) where


-- import qualified Data.Text       as T
-- import qualified Data.YAML.Token as Y
import           Data.YAML.Event       (Tag)
import           Data.YAML.Schema
import           Data.YAML.Loader

import           Util

-- | YAML Document node
data Node loc 
          = Scalar   !loc !Scalar 
          | Mapping  !loc !Tag (Map (Node loc) (Node loc)) 
          | Sequence !loc !Tag [Node loc] 
          | Anchor   !loc !NodeId !(Node loc) 
          deriving (Show)

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
