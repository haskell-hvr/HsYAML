{-# LANGUAGE Safe #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Event.Internal
    ( EvStream
    , Event(..)
    , Directives(..)
    , ScalarStyle(..)
    , NodeStyle(..)
    , scalarNodeStyle
    , Tag(..), untagged, isUntagged, tagToText
    , Anchor
    , Pos(..)
    , Y.Encoding(..)
    ) where


import qualified Data.Text       as T
import qualified Data.YAML.Token as Y

import           Util


-- | YAML Event Types
--
-- The events correspond to the ones from [LibYAML](http://pyyaml.org/wiki/LibYAML)
--
-- The grammar below defines well-formed streams of 'Event's:
--
-- @
-- stream   ::= 'StreamStart' document* 'StreamEnd'
-- document ::= 'DocumentStart' node 'DocumentEnd'
-- node     ::= 'Alias'
--            | 'Scalar'
--            | sequence
--            | mapping
-- sequence ::= 'SequenceStart' node* 'SequenceEnd'
-- mapping  ::= 'MappingStart' (node node)* 'MappingEnd'
-- @
--
-- @since 0.2.0
data Event
    = StreamStart
    | StreamEnd
    | DocumentStart  !Directives
    | DocumentEnd    !Bool
    | Alias          !Anchor
    | Scalar         !(Maybe Anchor)  !Tag  !ScalarStyle  !Text
    | SequenceStart  !(Maybe Anchor)  !Tag  !NodeStyle
    | SequenceEnd
    | MappingStart   !(Maybe Anchor)  !Tag  !NodeStyle
    | MappingEnd
    deriving (Show, Eq)

-- | Encodes document @%YAML@ directives and the directives end-marker
--
-- @since 0.2.0
data Directives = NoDirEndMarker    -- ^ no directives and also no @---@ marker
                | DirEndMarkerNoVersion -- ^ @---@ marker present, but no explicit @%YAML@ directive present
                | DirEndMarkerVersion !Word -- ^ @---@ marker present, as well as a @%YAML 1.mi@ version directive; the minor version @mi@ is stored in the 'Word' field.
                deriving (Show, Eq)

-- | 'Scalar'-specific node style
--
-- This can be considered a more granular superset of 'NodeStyle'.
-- See also 'scalarNodeStyle'.
--
-- @since 0.2.0
data ScalarStyle = Plain
                 | SingleQuoted
                 | DoubleQuoted
                 | Literal
                 | Folded
                 deriving (Eq,Ord,Show)

-- | Node style
--
-- @since 0.2.0
data NodeStyle = Flow
               | Block
               deriving (Eq,Ord,Show)

-- | Convert 'ScalarStyle' to 'NodeStyle'
--
-- @since 0.2.0
scalarNodeStyle :: ScalarStyle -> NodeStyle
scalarNodeStyle Plain        = Flow
scalarNodeStyle SingleQuoted = Flow
scalarNodeStyle DoubleQuoted = Flow
scalarNodeStyle Literal      = Block
scalarNodeStyle Folded       = Block

-- | YAML Anchor identifiers
type Anchor = Text

-- | YAML Tags
newtype Tag = Tag (Maybe Text)
            deriving (Eq,Ord)

instance Show Tag where
  show (Tag x) = show x


-- | Event stream produced by 'parseEvents'
--
-- A 'Left' value denotes parsing errors. The event stream ends
-- immediately once a 'Left' value is returned.
type EvStream = [Either (Pos,String) Event]

-- | Position in parsed YAML source
data Pos = Pos
    { posByteOffset :: !Int -- ^ 0-based byte offset
    , posCharOffset :: !Int -- ^ 0-based character (Unicode code-point) offset
    , posLine       :: !Int -- ^ 1-based line number
    , posColumn     :: !Int -- ^ 0-based character (Unicode code-point) column number
    } deriving Show


-- | Convert 'Tag' to its string representation
--
-- Returns 'Nothing' for 'untagged'
tagToText :: Tag -> Maybe T.Text
tagToText (Tag x) = x

-- | An \"untagged\" YAML tag
untagged :: Tag
untagged = Tag Nothing

-- | Equivalent to @(== 'untagged')@
isUntagged :: Tag -> Bool
isUntagged (Tag Nothing) = True
isUntagged _             = False

