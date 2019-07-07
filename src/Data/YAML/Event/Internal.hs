{-# LANGUAGE Safe #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Event.Internal
    ( EvStream
    , Event(..)
    , EvPos(..)
    , Directives(..)
    , ScalarStyle(..)
    , Chomp(..)
    , IndentOfs(..)
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
    | Comment        !Text
    | Alias          !Anchor
    | Scalar         !(Maybe Anchor)  !Tag  !ScalarStyle  !Text
    | SequenceStart  !(Maybe Anchor)  !Tag  !NodeStyle
    | SequenceEnd
    | MappingStart   !(Maybe Anchor)  !Tag  !NodeStyle
    | MappingEnd
    deriving (Show, Eq)

-- |'Event' with corresponding Pos in YAML stream
--
-- @since 0.2.0
data EvPos = EvPos{
    eEvent :: !Event,
    ePos   :: !Pos
}  deriving (Eq, Show)

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
                 | Literal !Chomp !IndentOfs
                 | Folded !Chomp !IndentOfs
                 deriving (Eq,Ord,Show)

-- | <https://yaml.org/spec/1.2/spec.html#id2794534 Block Chomping Indicator>
--  
-- @since 0.2.0
data Chomp = Strip -- ^ Remove all trailing line breaks and shows the presence of @-@ chomping indicator. 
           | Clip  -- ^ Keep first trailing line break; this also the default behavior used if no explicit chomping indicator is specified.
           | Keep  -- ^ Keep all trailing line breaks and shows the presence of @+@ chomping indicator.
           deriving (Eq,Ord,Show)

-- | Block Indentation Indicator
--
-- 'IndentAuto' is the special case for auto Block Indentation Indicator
--
-- @since 0.2.0
data IndentOfs = IndentAuto | IndentOfs1 | IndentOfs2 | IndentOfs3 | IndentOfs4 | IndentOfs5 | IndentOfs6 | IndentOfs7 | IndentOfs8 | IndentOfs9
                  deriving (Eq, Ord, Show, Enum)

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
scalarNodeStyle (Literal _ _)  = Block
scalarNodeStyle (Folded _ _ )  = Block

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
type EvStream = [Either (Pos,String) EvPos]


-- | Position in parsed YAML source
data Pos = Pos
    { posByteOffset :: !Int -- ^ 0-based byte offset
    , posCharOffset :: !Int -- ^ 0-based character (Unicode code-point) offset
    , posLine       :: !Int -- ^ 1-based line number
    , posColumn     :: !Int -- ^ 0-based character (Unicode code-point) column number
    }  deriving (Eq, Show)

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

