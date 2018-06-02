{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-3.0
--
-- Event-stream oriented YAML parsing API
module Data.YAML.Event
    ( parseEvents
    , EvStream
    , Event(..)
    , Style(..)
    , Tag
    , Anchor
    ) where

import           Control.Applicative  (Applicative (..))
import qualified Data.ByteString.Lazy as BS.L
import           Data.Char
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid          (Monoid (mappend, mempty))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.YAML.Token      as Y
import           Numeric              (readHex)

-- TODO: consider also non-essential attributes

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
data Event
    = StreamStart
    | StreamEnd
    | DocumentStart  !Bool
    | DocumentEnd    !Bool
    | Alias          !Anchor
    | Scalar         !(Maybe Anchor)  !Tag  !Style  !Text
    | SequenceStart  !(Maybe Anchor)  !Tag
    | SequenceEnd
    | MappingStart   !(Maybe Anchor)  !Tag
    | MappingEnd
    deriving (Show, Eq)

-- | YAML Anchor identifiers
type Anchor = Text

-- | YAML Tags
type Tag = Maybe Text

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

tok2pos :: Y.Token -> Pos
tok2pos Y.Token { Y.tByteOffset = posByteOffset, Y.tCharOffset = posCharOffset, Y.tLine = posLine, Y.tLineChar = posColumn } = Pos {..}

-- | 'Scalar' node style
data Style = Plain
           | SingleQuoted
           | DoubleQuoted
           | Literal
           | Folded
           deriving (Eq,Show)

-- internal
type TagHandle = Text
type Props = (Maybe Text,Tag)

tagUnescape :: Text -> Text
tagUnescape = T.pack . go . T.unpack
  where
    go [] = []
    go ('%':h:l:cs)
      | Just c <- decodeL1 [h,l] = c : go cs
    go (c:cs) = c : go cs

getHandle :: [Y.Token] -> Maybe (TagHandle,[Y.Token])
getHandle toks0 = do
  Y.Token { Y.tCode = Y.BeginHandle } : toks1 <- Just toks0
  (hs,Y.Token { Y.tCode = Y.EndHandle } : toks2) <- Just $ span (\Y.Token { Y.tCode = c } -> c `elem` [Y.Indicator,Y.Meta]) toks1
  pure (T.pack $ concatMap Y.tText hs, toks2)

getUriTag :: [Y.Token] -> Maybe (Text,[Y.Token])
getUriTag toks0 = do
  Y.Token { Y.tCode = Y.BeginTag } : toks1 <- Just toks0
  (hs,Y.Token { Y.tCode = Y.EndTag } : toks2) <- Just $ span (\Y.Token { Y.tCode = c } -> c `elem` [Y.Indicator,Y.Meta]) toks1
  pure (T.pack $ concatMap Y.tText hs, toks2)

-- | Parse YAML 'Event's from a lazy 'BS.L.ByteString'.
parseEvents :: BS.L.ByteString -> EvStream
parseEvents = \bs0 -> Right StreamStart : (go0 mempty $ stripComments $ filter (not . isWhite) (Y.tokenize bs0 False))
  where
    isTCode tc = (== tc) . Y.tCode
    skipPast tc (t : ts)
      | isTCode tc t = ts
      | otherwise = skipPast tc ts
    skipPast _ [] = error "the impossible happened"


    isWhite :: Y.Token -> Bool
    isWhite (Y.Token { Y.tCode = Y.White })  = True
    isWhite (Y.Token { Y.tCode = Y.Indent }) = True
    isWhite (Y.Token { Y.tCode = Y.Break })  = True
    isWhite _                                = False

    goDir :: Map TagHandle Text -> [Y.Token] -> EvStream
    goDir m (Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
             Y.Token { Y.tCode = Y.Meta, Y.tText = "YAML" } :
             Y.Token { Y.tCode = Y.Meta } :
             Y.Token { Y.tCode = Y.EndDirective } :
             rest) = go0 m rest

    goDir m (Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
             Y.Token { Y.tCode = Y.Meta, Y.tText = "TAG" } :
             rest)
      | Just (h, rest') <- getHandle rest
      , Just (t, rest'') <- getUriTag rest' = go0 (Map.insert h t m) (skipPast Y.EndDirective rest'')

    goDir m (Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
             Y.Token { Y.tCode = Y.Meta, Y.tText = l } :
             rest) | l `notElem` ["TAG","YAML"] = go0 m (skipPast Y.EndDirective rest)
    goDir _ xs                                            = err xs

    go0 :: Map.Map TagHandle Text -> Tok2EvStream
    go0 _ [] = [Right StreamEnd]
    go0 _ (Y.Token { Y.tCode = Y.White } : _) = error "the impossible happened"
    go0 m (Y.Token { Y.tCode = Y.Indicator } : rest) = go0 m rest -- ignore indicators here
    go0 m (Y.Token { Y.tCode = Y.DirectivesEnd } : rest) = go0 m rest
    go0 m (Y.Token { Y.tCode = Y.BeginDocument } : Y.Token { Y.tCode = Y.DirectivesEnd } : rest) = Right (DocumentStart True) : go0 m rest -- hack
    go0 m (Y.Token { Y.tCode = Y.BeginDocument } : rest@(Y.Token { Y.tCode = Y.BeginDirective } : _)) = Right (DocumentStart True) : go0 m rest -- hack
    go0 m (Y.Token { Y.tCode = Y.BeginDocument } : rest) = Right (DocumentStart False) : go0 m rest
    go0 _ (Y.Token { Y.tCode = Y.EndDocument } : Y.Token { Y.tCode = Y.DocumentEnd } : rest) = Right (DocumentEnd True) : go0 mempty rest
    go0 _ (Y.Token { Y.tCode = Y.EndDocument } : rest) = Right (DocumentEnd False) : go0 mempty rest
    go0 m (Y.Token { Y.tCode = Y.DocumentEnd } : rest) = go0 m rest -- should not occur
    go0 m (Y.Token { Y.tCode = Y.BeginNode } : rest) = goNode0 m rest (go0 m)
    go0 m (Y.Token { Y.tCode = Y.BeginDirective } : rest) = goDir m rest
    go0 _ xs = err xs

err :: Tok2EvStream
err (tok@Y.Token { Y.tCode = Y.Error, Y.tText = msg } : _) = [Left (tok2pos tok, msg)]
err (tok : _) = [Left (tok2pos tok, "Internal error")]
err [] = [Left ((Pos (-1) (-1) (-1) (-1)), "Unexpected end of token stream")]

goNode0 :: Map TagHandle Text -> Tok2EvStreamCont
goNode0 tagmap = goNode
  where
    goNode :: Tok2EvStreamCont
    goNode (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont = goScalar mempty rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginSequence } : rest) cont = Right (SequenceStart Nothing Nothing) : goSeq rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginMapping }  : rest) cont = Right (MappingStart Nothing Nothing) : goMap rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginProperties } : rest) cont = goProp mempty rest (\p rest' -> goNode' p rest' cont)
    goNode (Y.Token { Y.tCode = Y.BeginAlias } :
            Y.Token { Y.tCode = Y.Indicator } :
            Y.Token { Y.tCode = Y.Meta, Y.tText = anchor } :
            Y.Token { Y.tCode = Y.EndAlias } :
            Y.Token { Y.tCode = Y.EndNode } :
            rest) cont = Right (Alias (T.pack anchor)) : cont rest
    goNode xs _cont = err xs

    goNode' :: Props -> Tok2EvStreamCont
    goNode' props (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont   = goScalar props rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (Y.Token { Y.tCode = Y.BeginSequence } : rest) cont = Right (SequenceStart manchor mtag) : goSeq rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (Y.Token { Y.tCode = Y.BeginMapping }  : rest) cont = Right (MappingStart manchor mtag) : goMap rest (flip goNodeEnd cont)
    goNode' _ xs                                            _cont = err xs

    goNodeEnd :: Tok2EvStreamCont
    goNodeEnd (Y.Token { Y.tCode = Y.EndNode } : rest) cont = cont rest
    goNodeEnd xs                                      _cont = err xs

    goProp :: Props -> [Y.Token] -> (Props -> [Y.Token] -> EvStream) -> EvStream
    goProp props (Y.Token { Y.tCode = Y.EndProperties } : rest) cont = cont props rest
    goProp props (Y.Token { Y.tCode = Y.BeginAnchor } : rest) cont = goAnchor props rest (\x y -> goProp x y cont)
    goProp props (Y.Token { Y.tCode = Y.BeginTag } : rest) cont = goTag props rest (\x y -> goProp x y cont)
    goProp _props xs                                     _cont = err xs

    goAnchor :: Props -> [Y.Token] -> (Props -> [Y.Token] -> EvStream) -> EvStream
    goAnchor props (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goAnchor props rest cont
    goAnchor (_,tag) (Y.Token { Y.tCode = Y.Meta, Y.tText = anchor } : rest) cont = goAnchor (Just $! T.pack anchor,tag) rest cont
    goAnchor props (Y.Token { Y.tCode = Y.EndAnchor } : rest) cont = cont props rest
    goAnchor _ xs _ = err xs

    goTag :: Props -> [Y.Token] -> (Props -> [Y.Token] -> EvStream) -> EvStream

    goTag (anchor,_) (Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont = cont (anchor,Just $! T.pack "!") rest

    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont
            | Just t' <- Map.lookup (T.pack ("!!")) tagmap
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = cont (anchor,Just $! T.pack ("tag:yaml.org,2002:" ++ tag)) rest

    goTag (anchor,_) (Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "<" } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = ">" } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont = cont (anchor,mkTag tag) rest

    goTag (anchor,_) xs@(Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = h } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont
            | Just t' <- Map.lookup (T.pack ("!" ++ h ++ "!")) tagmap
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = err xs

    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont
            | Just t' <- Map.lookup (T.pack ("!")) tagmap
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = cont (anchor,Just $! T.pack ('!' : tag)) rest -- unresolved
    goTag _ xs _ = err xs

    mkTag = Just . tagUnescape . T.pack

    goScalar :: Props -> Tok2EvStreamCont
    goScalar (manchor,tag) toks0 cont = go' "" Plain toks0
      where
        go' acc sty (Y.Token { Y.tCode = Y.Text, Y.tText = t } : rest) = go'  (acc ++ t) sty rest
        go' acc sty (Y.Token { Y.tCode = Y.LineFold } : rest) = go'  (acc ++ " ") sty rest
        go' acc sty (Y.Token { Y.tCode = Y.LineFeed } : rest) = go'  (acc ++ "\n") sty rest

        go' acc sty (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "'" } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = "'" } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest) = go'  (acc ++ "'") sty rest

        go' acc sty (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
--                     Y.Token { Y.tCode = Y.Break } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest) = go' acc sty rest -- end-line

        go' acc sty (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = t } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest)
          | t == "n"  = go'  (acc ++ "\n") sty rest
          | t == "r"  = go'  (acc ++ "\r") sty rest
          | t == "t"  = go'  (acc ++ "\t") sty rest
          | t == "b"  = go'  (acc ++ "\b") sty rest
          | t == "/"  = go'  (acc ++ t) sty rest
          | t == " "  = go'  (acc ++ t) sty rest
          | t == "\\"  = go'  (acc ++ t) sty rest
          | t == "\"" = go'  (acc ++ t) sty rest

        go' acc sty (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = pfx } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = ucode } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest)
          | pfx == "u", Just c <- decodeCP ucode = go' (acc ++ [c]) sty rest
          | pfx == "x", Just c <- decodeL1 ucode = go' (acc ++ [c]) sty rest

        go' acc sty (Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest)
          | "'"  <- ind = go' acc SingleQuoted rest
          | "\"" <- ind = go' acc DoubleQuoted rest
          | "|"  <- ind = go' acc Literal rest
          | ">"  <- ind = go' acc Folded rest
          | otherwise   = go' acc sty rest
        go' acc sty (Y.Token { Y.tCode = Y.EndScalar } : rest) = Right (Scalar manchor tag sty (T.pack acc)) : cont rest
        go' _ _ xs | False = error (show xs)
        go' _ _ xs = err xs

    goSeq :: Tok2EvStreamCont
    goSeq (Y.Token { Y.tCode = Y.EndSequence } : rest) cont = Right SequenceEnd : cont rest
    goSeq (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.BeginMapping } : rest) cont = Right (MappingStart Nothing Nothing) : goMap rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goSeq rest cont
--    goSeq xs _cont = error (show xs)
    goSeq xs _cont = err xs

    goMap :: Tok2EvStreamCont
    goMap (Y.Token { Y.tCode = Y.EndMapping } : rest) cont = Right MappingEnd : cont rest
    goMap (Y.Token { Y.tCode = Y.BeginPair } : rest) cont = goPair1 rest (flip goMap cont)
    goMap (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goMap rest cont
    goMap xs _cont = err xs

    goPair1 (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goPair2 cont)
    goPair1 (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goPair1 rest cont
    goPair1 xs _cont = err xs

    goPair2 (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goPairEnd cont)
    goPair2 (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goPair2 rest cont
    goPair2 xs _cont                                        = err xs

    goPairEnd (Y.Token { Y.tCode = Y.EndPair } : rest) cont = cont rest
    goPairEnd xs _cont                                      = err xs


stripComments :: [Y.Token] -> [Y.Token]
stripComments (Y.Token { Y.tCode = Y.BeginComment } : rest) = skip rest
  where
    skip (Y.Token { Y.tCode = Y.EndComment } : rest') = stripComments rest'
    skip (_                                  : rest') = skip rest'
    skip [] = error "the impossible happened"
stripComments (t : rest) = t : stripComments rest
stripComments [] = []

type Tok2EvStream = [Y.Token] -> EvStream

type Tok2EvStreamCont = [Y.Token] -> Cont EvStream [Y.Token]

type Cont r a = (a -> r) -> r


-- decode 4-hex-digit unicode code-point
decodeCP :: String -> Maybe Char
decodeCP s = case s of
               [_,_,_,_] | all isHexDigit s
                         , [(j, "")] <- readHex s -> Just (chr (fromInteger j))
               _ -> Nothing

-- decode 2-hex-digit latin1 code-point
decodeL1 :: String -> Maybe Char
decodeL1 s = case s of
               [_,_] | all isHexDigit s
                     , [(j, "")] <- readHex s -> Just (chr (fromInteger j))
               _ -> Nothing
