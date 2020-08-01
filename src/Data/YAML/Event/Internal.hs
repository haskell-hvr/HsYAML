{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

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
import           Data.YAML.Pos   (Pos (..))
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
--            | 'Comment'
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
    | EmptyLine
    | DocumentStart  !Directives
    | DocumentEnd    !Bool
    | Comment        !Text
    | Alias          !Anchor
    | Scalar         !(Maybe Anchor)  !Tag  !ScalarStyle  !Text
    | SequenceStart  !(Maybe Anchor)  !Tag  !NodeStyle
    | SequenceEnd
    | MappingStart   !(Maybe Anchor)  !Tag  !NodeStyle
    | MappingEnd
    deriving (Show, Eq, Generic)

-- | @since 0.2.0
instance NFData Event where
  rnf  StreamStart          = ()
  rnf  StreamEnd            = ()
  rnf  EmptyLine            = ()
  rnf (DocumentStart _)     = ()
  rnf (DocumentEnd _)       = ()
  rnf (Comment _)           = ()
  rnf (Alias _)             = ()
  rnf (Scalar a _ _ _)      = rnf a
  rnf (SequenceStart a _ _) = rnf a
  rnf  SequenceEnd          = ()
  rnf (MappingStart a _ _)  = rnf a
  rnf  MappingEnd           = ()

-- |'Event' with corresponding Pos in YAML stream
--
-- @since 0.2.0
data EvPos = EvPos
  { eEvent :: !Event
  , ePos   :: !Pos
  }  deriving (Eq, Show, Generic)

-- | @since 0.2.0
instance NFData EvPos where rnf (EvPos ev p) = rnf (ev,p)

-- | Encodes document @%YAML@ directives and the directives end-marker
--
-- @since 0.2.0
data Directives = NoDirEndMarker    -- ^ no directives and also no @---@ marker
                | DirEndMarkerNoVersion -- ^ @---@ marker present, but no explicit @%YAML@ directive present
                | DirEndMarkerVersion !Word -- ^ @---@ marker present, as well as a @%YAML 1.mi@ version directive; the minor version @mi@ is stored in the 'Word' field.
                deriving (Show, Eq, Generic)

-- | @since 0.2.0
instance NFData Directives where rnf !_ = ()

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
                 deriving (Eq,Ord,Show,Generic)

-- | @since 0.2.0
instance NFData ScalarStyle where rnf !_ = ()

-- | <https://yaml.org/spec/1.2/spec.html#id2794534 Block Chomping Indicator>
--
-- @since 0.2.0
data Chomp = Strip -- ^ Remove all trailing line breaks and shows the presence of @-@ chomping indicator.
           | Clip  -- ^ Keep first trailing line break; this also the default behavior used if no explicit chomping indicator is specified.
           | Keep  -- ^ Keep all trailing line breaks and shows the presence of @+@ chomping indicator.
           deriving (Eq,Ord,Show,Generic)

-- | @since 0.2.0
instance NFData Chomp where rnf !_ = ()

-- | Block Indentation Indicator
--
-- 'IndentAuto' is the special case for auto Block Indentation Indicator
--
-- @since 0.2.0
data IndentOfs = IndentAuto | IndentOfs1 | IndentOfs2 | IndentOfs3 | IndentOfs4 | IndentOfs5 | IndentOfs6 | IndentOfs7 | IndentOfs8 | IndentOfs9
                  deriving (Eq, Ord, Show, Enum, Generic)

-- | @since 0.2.0
instance NFData IndentOfs where rnf !_ = ()

-- | Node style
--
-- @since 0.2.0
data NodeStyle = Flow
               | Block
               deriving (Eq,Ord,Show,Generic)

-- | @since 0.2.0
instance NFData NodeStyle where rnf !_ = ()

-- | Convert 'ScalarStyle' to 'NodeStyle'
--
-- @since 0.2.0
scalarNodeStyle :: ScalarStyle -> NodeStyle
scalarNodeStyle Plain         = Flow
scalarNodeStyle SingleQuoted  = Flow
scalarNodeStyle DoubleQuoted  = Flow
scalarNodeStyle (Literal _ _) = Block
scalarNodeStyle (Folded _ _ ) = Block

-- | YAML Anchor identifiers
type Anchor = Text

-- | YAML Tags
newtype Tag = Tag (Maybe Text)
            deriving (Eq,Ord,Generic)

instance Show Tag where
  show (Tag x) = show x

-- | @since 0.2.0
instance NFData Tag where rnf (Tag x) = rnf x

-- | Event stream produced by 'Data.YAML.Event.parseEvents'
--
-- A 'Left' value denotes parsing errors. The event stream ends
-- immediately once a 'Left' value is returned.
type EvStream = [Either (Pos,String) EvPos]


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

