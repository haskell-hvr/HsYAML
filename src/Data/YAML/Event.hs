{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Event-stream oriented YAML parsing and serializing API
module Data.YAML.Event
    (
      -- * Tutorial
      -- $start

      -- ** Parsing YAML Documents
      -- $parsing
      parseEvents

      -- ** Serializing Events to YAML Character Stream
      -- $serialize
    , writeEvents
    , writeEventsText

      -- ** How to comment your yaml document for best results
      -- $commenting

      -- ** Event-stream Internals
    , EvStream
    , Event(..)
    , EvPos(..)
    , Directives(..)
    , ScalarStyle(..)
    , NodeStyle(..)
    , Chomp(..)
    , IndentOfs(..)
    , Tag, untagged, isUntagged, tagToText, mkTag
    , Anchor
    , Pos(..)
    ) where

import           Data.YAML.Event.Internal
import           Data.YAML.Event.Writer   (writeEvents, writeEventsText)

import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.Char                as C
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.YAML.Token          as Y
import           Numeric                  (readHex)

import           Util

-- | Construct YAML tag
mkTag :: String -> Tag
mkTag "" = error "mkTag"
mkTag "!" = Tag (Just $! T.pack "!")
mkTag s   = Tag (Just $! tagUnescape s)
  where
    tagUnescape = T.pack . go
      where
        go [] = []
        go ('%':h:l:cs)
          | Just c <- decodeL1 [h,l] = c : go cs
        go (c:cs) = c : go cs


mkTag' :: String -> Tag
mkTag' "" = error "mkTag'"
mkTag' s  = Tag (Just $! T.pack s)

mkTag'' :: String -> Tag
mkTag'' "" = error "mkTag''"
mkTag'' s  = Tag (Just $! T.pack ("tag:yaml.org,2002:" ++ s))

-- Returns the position corresponding to the 'Token'
tok2pos :: Y.Token -> Pos
tok2pos Y.Token { Y.tByteOffset = posByteOffset, Y.tCharOffset = posCharOffset, Y.tLine = posLine, Y.tLineChar = posColumn } = Pos {..}

-- Construct a 'EvPos' from the given 'Event' and 'Pos'
getEvPos :: Event -> Y.Token -> EvPos
getEvPos ev tok = EvPos { eEvent = ev , ePos = tok2pos tok }

-- Initial position('Pos' corresponding to the 'StreamStart')
initPos :: Pos
initPos = Pos { posByteOffset = 0 , posCharOffset = 0  , posLine = 1 , posColumn = 0 }

-- internal
type TagHandle = Text
type Props = (Maybe Text,Tag)

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

{- WARNING: the code that follows will make you cry; a safety pig is provided below for your benefit.

                         _
 _._ _..._ .-',     _.._(`))
'-. `     '  /-._.-'    ',/
   )         \            '.
  / _    _    |             \
 |  a    a    /              |
 \   .-.                     ;
  '-('' ).-'       ,'       ;
     '-;           |      .'
        \           \    /
        | 7  .__  _.-\   \
        | |  |  ``/  /`  /
       /,_|  |   /,_/   /
          /,_/      '`-'

-}

fixUpEOS :: EvStream -> EvStream
fixUpEOS = go initPos
  where
    go :: Pos -> EvStream -> EvStream
    go _ []                          = []
    go p [Right (EvPos StreamEnd _)] = [Right (EvPos StreamEnd p)]
    go _ (e@(Right (EvPos _ p)):es)  = e : go p es
    go _ (e@(Left (p,_)):es)         = e : go p es

-- | Parse YAML 'Event's from a lazy 'BS.L.ByteString'.
--
-- The parsed Events allow us to round-trip at the event-level while preserving many features and presentation details like
-- 'Comment's,'ScalarStyle','NodeStyle', 'Anchor's, 'Directives' marker along with YAML document version,
-- 'Chomp'ing Indicator,Indentation Indicator ('IndentOfs') ,ordering, etc.
-- It does not preserve non-content white spaces.
--
-- The input 'BS.L.ByteString' is expected to have a YAML 1.2 stream
-- using the UTF-8, UTF-16 (LE or BE), or UTF-32 (LE or BE) encodings
-- (which will be auto-detected).
--
parseEvents :: BS.L.ByteString -> EvStream
parseEvents = \bs0 -> fixUpEOS $ Right (EvPos StreamStart initPos) : (go0 $ filter (not . isWhite) $ Y.tokenize bs0 False)
  where
    isTCode tc = (== tc) . Y.tCode
    skipPast tc (t : ts)
      | isTCode tc t = ts
      | otherwise = skipPast tc ts
    skipPast _ [] = error "the impossible happened"

    -- non-content whitespace
    isWhite :: Y.Token -> Bool
    isWhite (Y.Token { Y.tCode = Y.Bom   })  = True -- BOMs can occur at each doc-start!
    isWhite (Y.Token { Y.tCode = Y.White })  = True
    isWhite (Y.Token { Y.tCode = Y.Indent }) = True
    isWhite (Y.Token { Y.tCode = Y.Break })  = True
    isWhite _                                = False


    go0 :: Tok2EvStream
    go0 []                                                = [Right (EvPos StreamEnd initPos {- fixed up by fixUpEOS -} )]
    go0 toks0@(Y.Token { Y.tCode = Y.BeginComment} : _)   = goComment toks0 go0
    go0 toks0@(Y.Token { Y.tCode = Y.BeginDocument } : _) = go1 dinfo0 toks0
    go0 (Y.Token { Y.tCode = Y.DocumentEnd } : rest)      = go0 rest -- stray/redundant document-end markers cause this
    go0 xs                                                = err xs


    go1 :: DInfo -> Tok2EvStream
    go1 m (Y.Token { Y.tCode = Y.BeginDocument } : rest) = goDirs m rest
    go1 _ (tok@Y.Token { Y.tCode = Y.EndDocument } : Y.Token { Y.tCode = Y.DocumentEnd } : rest) = ( Right (getEvPos (DocumentEnd True) tok )): go0 rest
    go1 _ (tok@Y.Token { Y.tCode = Y.EndDocument } : rest) = ( Right (getEvPos (DocumentEnd False) tok )) : go0 rest
    go1 m toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) = goComment toks0 (go1 m)
    go1 m (Y.Token { Y.tCode = Y.BeginNode } : rest) = goNode0 m rest (go1 m)
    go1 _ xs = err xs

    -- consume {Begin,End}Directives and emit DocumentStart event
    goDirs :: DInfo -> Tok2EvStream
    goDirs m (Y.Token { Y.tCode = Y.BeginDirective } : rest) = goDir1 m rest
    goDirs m toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) = goComment toks0 (goDirs m)
    goDirs m (tok@Y.Token { Y.tCode = Y.DirectivesEnd } : rest)
      | Just (1,mi) <- diVer m = Right (getEvPos (DocumentStart (DirEndMarkerVersion mi)) tok) : go1 m rest
      | otherwise              = Right (getEvPos (DocumentStart DirEndMarkerNoVersion) tok) : go1 m rest
    goDirs _ xs@(Y.Token { Y.tCode = Y.BeginDocument } : _) = err xs
    goDirs m xs = Right ( getEvPos (DocumentStart NoDirEndMarker) (head xs) ): go1 m xs

    -- single directive
    goDir1 :: DInfo -> [Y.Token] -> EvStream
    goDir1 m toks0@(Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
                    Y.Token { Y.tCode = Y.Meta, Y.tText = "YAML" } :
                    Y.Token { Y.tCode = Y.Meta, Y.tText = v } :
                    Y.Token { Y.tCode = Y.EndDirective } :
                    rest)
      | diVer m /= Nothing = errMsg "Multiple %YAML directives" toks0
      | Just (1,mi) <- decodeVer v = goDirs (m { diVer = Just (1,mi) }) rest -- TODO: warn for non-1.2
      | otherwise = errMsg ("Unsupported YAML version " <> show v) toks0

    goDir1 m toks0@(Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
                    Y.Token { Y.tCode = Y.Meta, Y.tText = "TAG" } :
                    rest)
      | Just (h, rest') <- getHandle rest
      , Just (t, rest'') <- getUriTag rest' = case mapInsertNoDupe h t (diTags m) of
                                                Just tm  -> goDirs (m { diTags = tm }) (skipPast Y.EndDirective rest'')
                                                Nothing  -> errMsg ("Multiple %TAG definitions for handle " <> show h) toks0

    goDir1 m (Y.Token { Y.tCode = Y.Indicator, Y.tText = "%" } :
             Y.Token { Y.tCode = Y.Meta, Y.tText = l } :
             rest) | l `notElem` ["TAG","YAML"] = goDirs m (skipPast Y.EndDirective rest)
    goDir1 _ xs                                            = err xs

    -- | Decode versions of the form @<major>.<minor>@
    decodeVer :: String -> Maybe (Word,Word)
    decodeVer s = do
      (lhs,'.':rhs) <- Just (break (=='.') s)
      (,) <$> readMaybe lhs <*> readMaybe rhs

data DInfo = DInfo { diTags :: Map.Map TagHandle Text
                   , diVer  :: Maybe (Word,Word)
                   }

dinfo0 :: DInfo
dinfo0 = DInfo mempty Nothing

errMsg :: String -> Tok2EvStream
errMsg msg (tok : _) = [Left (tok2pos tok, msg)]
errMsg msg [] = [Left ((Pos (-1) (-1) (-1) (-1)), ("Unexpected end of token stream: " <> msg))]

err :: Tok2EvStream
err (tok@Y.Token { Y.tCode = Y.Error, Y.tText = msg } : _) = [Left (tok2pos tok, msg)]
err (tok@Y.Token { Y.tCode = Y.Unparsed, Y.tText = txt } : _) = [Left (tok2pos tok, ("Lexical error near " ++ show txt))]
err (tok@Y.Token { Y.tCode = code } : _) = [Left (tok2pos tok, ("Parse failure near " ++ show code ++ " token"))]
err [] = [Left ((Pos (-1) (-1) (-1) (-1)), "Unexpected end of token stream")]

goNode0 :: DInfo -> Tok2EvStreamCont
goNode0 DInfo {..} = goNode
  where
    seqInd "[" = Flow
    seqInd "-" = Block
    seqInd _   = error "seqInd: internal error" -- impossible

    mapInd "{" = Flow
    mapInd _   = error "mapInd: internal error" -- impossible

    goNode :: Tok2EvStreamCont
    goNode toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goNode cont)
    goNode (tok@Y.Token { Y.tCode = Y.BeginScalar } : rest) cont = goScalar (tok2pos tok) (mempty,untagged) rest (flip goNodeEnd cont)
    goNode (tok@Y.Token { Y.tCode = Y.BeginSequence } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (getEvPos (SequenceStart Nothing untagged (seqInd ind)) tok): goSeq rest (flip goNodeEnd cont)
    goNode (tok@Y.Token { Y.tCode = Y.BeginMapping }  : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (getEvPos (MappingStart Nothing untagged (mapInd ind)) tok) : goMap rest (flip goNodeEnd cont)
    goNode (tok@Y.Token { Y.tCode = Y.BeginMapping }  : rest) cont = Right (getEvPos (MappingStart Nothing untagged Block) tok) : goMap rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginProperties } : rest) cont = goProp (mempty,untagged) rest (\p rest' -> goNode' p rest' cont)
    goNode (tok@Y.Token { Y.tCode = Y.BeginAlias } :
            Y.Token { Y.tCode = Y.Indicator } :
            Y.Token { Y.tCode = Y.Meta, Y.tText = anchor } :
            Y.Token { Y.tCode = Y.EndAlias } :
            Y.Token { Y.tCode = Y.EndNode } :
            rest) cont = Right (getEvPos (Alias (T.pack anchor)) tok) : cont rest
    goNode xs _cont = err xs

    goNode' :: Props -> Tok2EvStreamCont
    goNode' props toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont  = goComment toks0 (flip (goNode' props) cont)
    goNode' props (tok@Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont   = goScalar (tok2pos tok) props rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (tok@Y.Token { Y.tCode = Y.BeginSequence } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (getEvPos (SequenceStart manchor mtag (seqInd ind)) tok) : goSeq rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (tok@Y.Token { Y.tCode = Y.BeginMapping }  : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (getEvPos (MappingStart manchor mtag (mapInd ind)) tok) : goMap rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (tok@Y.Token { Y.tCode = Y.BeginMapping } : rest) cont = Right (getEvPos (MappingStart manchor mtag Block) tok) : goMap rest (flip goNodeEnd cont)
    goNode' _ xs                                            _cont = err xs

    goNodeEnd :: Tok2EvStreamCont
    goNodeEnd toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goNodeEnd cont)
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
          cont = cont (anchor,mkTag' "!") rest

    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont
            | Just t' <- Map.lookup (T.pack ("!!")) diTags
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = cont (anchor,mkTag'' tag) rest

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
            | Just t' <- Map.lookup (T.pack ("!" ++ h ++ "!")) diTags
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = err xs

    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont
            | Just t' <- Map.lookup (T.pack ("!")) diTags
              = cont (anchor,mkTag (T.unpack t' ++ tag)) rest
            | otherwise = cont (anchor,mkTag' ('!' : tag)) rest -- unresolved
    goTag _ xs _ = err xs

    goScalar :: Pos -> Props -> Tok2EvStreamCont
    goScalar pos0 (manchor,tag) toks0 cont = go0 False Plain toks0
      where
        go0 ii sty (Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest)
          | "'"  <- ind = go' ii "" SingleQuoted rest
          | "\"" <- ind = go' ii "" DoubleQuoted rest
          | "|"  <- ind = go0 True (Literal Clip IndentAuto) rest
          | ">"  <- ind = go0 True (Folded Clip IndentAuto) rest

          | "+"  <- ind = go0 ii (chn sty Keep) rest
          | "-"  <- ind = go0 ii (chn sty Strip) rest
          | [c]  <- ind, '1' <= c, c <= '9' = go0 False (chn' sty (C.digitToInt c)) rest

        go0 ii sty tok@(Y.Token { Y.tCode = Y.BeginComment} : _)           = goComment tok (go0 ii sty)
        go0 ii sty (Y.Token { Y.tCode = Y.Text, Y.tText = t } : rest)      = go' ii t sty rest
        go0 ii sty (Y.Token { Y.tCode = Y.LineFold } : rest)               = go' ii " " sty rest
        go0 ii sty (Y.Token { Y.tCode = Y.LineFeed } : rest)               = go' ii "\n" sty rest
        go0 _  sty (Y.Token { Y.tCode = Y.EndScalar } : rest)          = Right (EvPos (Scalar manchor tag sty mempty) pos0) : cont rest

        go0 _ _ xs = err xs

        chn :: ScalarStyle -> Chomp -> ScalarStyle
        chn (Literal _ digit) chmp = Literal chmp digit
        chn (Folded _ digit) chmp  = Folded chmp digit
        chn _ _                    = error "impossible"

        chn' :: ScalarStyle -> Int -> ScalarStyle
        chn' (Literal b _) digit = Literal b (toEnum digit)
        chn' (Folded b _) digit  = Folded b (toEnum digit)
        chn' _ _                 = error "impossible"

        ----------------------------------------------------------------------------

        go' ii acc sty (Y.Token { Y.tCode = Y.Text, Y.tText = t } : rest) = go' ii (acc ++ t) sty rest
        go' ii acc sty (Y.Token { Y.tCode = Y.LineFold } : rest) = go' ii (acc ++ " ") sty rest
        go' ii acc sty (Y.Token { Y.tCode = Y.LineFeed } : rest) = go' ii (acc ++ "\n") sty rest

        go' ii acc sty@SingleQuoted
                    (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "'" } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = "'" } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest) = go' ii (acc ++ "'") sty rest

        go' ii acc sty@SingleQuoted
                    (Y.Token { Y.tCode = Y.Indicator, Y.tText = "'" } :
                     rest) = go' ii acc sty rest

        go' ii acc sty@DoubleQuoted
                    (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
--                     Y.Token { Y.tCode = Y.Break } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest) = go' ii acc sty rest -- line continuation escape

        go' ii acc sty@DoubleQuoted
                    (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = t } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest)
          | Just t' <- unescape t = go' ii (acc ++ t') sty rest

        go' ii acc sty@DoubleQuoted
                    (Y.Token { Y.tCode = Y.BeginEscape } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = "\\" } :
                     Y.Token { Y.tCode = Y.Indicator, Y.tText = pfx } :
                     Y.Token { Y.tCode = Y.Meta, Y.tText = ucode } :
                     Y.Token { Y.tCode = Y.EndEscape } :
                     rest)
          | pfx == "U", Just c <- decodeCP2 ucode = go' ii (acc ++ [c]) sty rest
          | pfx == "u", Just c <- decodeCP  ucode = go' ii (acc ++ [c]) sty rest
          | pfx == "x", Just c <- decodeL1  ucode = go' ii (acc ++ [c]) sty rest

        go' ii acc sty@DoubleQuoted
                    (Y.Token { Y.tCode = Y.Indicator, Y.tText = "\"" } :
                     rest) = go' ii acc sty rest

        go' ii acc sty (t@Y.Token { Y.tCode = Y.EndScalar } :
                     rest)
          | ii, hasLeadingSpace acc = [Left (tok2pos t, "leading empty lines contain more spaces than the first non-empty line in scalar: " ++ show acc)]
          | otherwise = Right (EvPos (Scalar manchor tag sty (T.pack acc)) pos0) : cont rest

        go' _ _ _ xs | False = error (show xs)
        go' _ _ _ xs = err xs

        hasLeadingSpace (' ':_)   = True
        hasLeadingSpace ('\n':cs) = hasLeadingSpace cs
        hasLeadingSpace _         = False

    goSeq :: Tok2EvStreamCont
    goSeq (tok@Y.Token { Y.tCode = Y.EndSequence } : rest) cont = Right (getEvPos SequenceEnd  tok): cont rest
    goSeq toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goSeq cont)
    goSeq (tok@Y.Token { Y.tCode = Y.BeginMapping } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } :  rest) cont = Right (getEvPos (MappingStart Nothing untagged (mapInd ind)) tok) : goMap rest (flip goSeq cont)
    goSeq (tok@Y.Token { Y.tCode = Y.BeginMapping } : rest) cont = Right (getEvPos (MappingStart Nothing untagged Block) tok) : goMap rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goSeq rest cont
--    goSeq xs _cont = error (show xs)
    goSeq xs _cont = err xs

    goMap :: Tok2EvStreamCont
    goMap (tok@Y.Token { Y.tCode = Y.EndMapping } : rest) cont = Right (getEvPos MappingEnd tok) : cont rest
    goMap toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goMap cont)
    goMap (Y.Token { Y.tCode = Y.BeginPair } : rest) cont = goPair1 rest (flip goMap cont)
    goMap (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goMap rest cont
    goMap xs _cont = err xs

    goPair1 (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goPair2 cont)
    goPair1 toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goPair1 cont)
    goPair1 (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goPair1 rest cont
    goPair1 xs _cont = err xs

    goPair2 toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goPair2 cont)
    goPair2 (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goPairEnd cont)
    goPair2 (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goPair2 rest cont
    goPair2 xs _cont                                        = err xs

    goPairEnd toks0@(Y.Token { Y.tCode = Y.BeginComment} : _) cont = goComment toks0 (flip goPairEnd cont)
    goPairEnd (Y.Token { Y.tCode = Y.EndPair } : rest) cont = cont rest
    goPairEnd xs _cont                                      = err xs


goComment :: Tok2EvStreamCont
goComment (tok@Y.Token { Y.tCode = Y.BeginComment} :
          Y.Token { Y.tCode = Y.Indicator, Y.tText = "#" } :
          Y.Token { Y.tCode = Y.Meta, Y.tText = comment } :
          Y.Token { Y.tCode = Y.EndComment } : rest) cont   = (Right (getEvPos (Comment (T.pack comment)) tok)) : cont rest
goComment (tok@Y.Token { Y.tCode = Y.BeginComment} :
          Y.Token { Y.tCode = Y.Indicator, Y.tText = "#" } :
          Y.Token { Y.tCode = Y.EndComment } : rest) cont   = (Right (getEvPos (Comment T.empty) tok)) : cont rest
goComment xs _cont = err xs

-- stripComments :: [Y.Token] -> [Y.Token]
-- stripComments (Y.Token { Y.tCode = Y.BeginComment } : rest) = skip rest
--   where
--     skip (Y.Token { Y.tCode = Y.EndComment } : rest') = stripComments rest'
--     skip (_                                  : rest') = skip rest'
--     skip [] = error "the impossible happened"
-- stripComments (t : rest) = t : stripComments rest
-- stripComments [] = []

type Tok2EvStream = [Y.Token] -> EvStream

type Tok2EvStreamCont = [Y.Token] -> Cont EvStream [Y.Token]

type Cont r a = (a -> r) -> r


-- decode 8-hex-digit unicode code-point
decodeCP2 :: String -> Maybe Char
decodeCP2 s = case s of
               [_,_,_,_,_,_,_,_] | all C.isHexDigit s
                                 , [(j, "")] <- readHex s -> Just (chr (fromInteger j))
               _ -> Nothing

-- decode 4-hex-digit unicode code-point
decodeCP :: String -> Maybe Char
decodeCP s = case s of
               [_,_,_,_] | all C.isHexDigit s
                         , [(j, "")] <- readHex s -> Just (chr (fromInteger j))
               _ -> Nothing

-- decode 2-hex-digit latin1 code-point
decodeL1 :: String -> Maybe Char
decodeL1 s = case s of
               [_,_] | all C.isHexDigit s
                     , [(j, "")] <- readHex s -> Just (chr (fromInteger j))
               _ -> Nothing

-- decode C-style escapes
unescape :: String -> Maybe String
unescape [c] = Map.lookup c m
  where
    m = Map.fromList [ (k,[v]) | (k,v) <- escapes ]

    escapes :: [(Char,Char)]
    escapes =
      [ ('0',   '\0')
      , ('a',   '\x7')
      , ('b',   '\x8')
      , ('\x9', '\x9')
      , ('t',   '\x9')
      , ('n',   '\xa')
      , ('v',   '\xb')
      , ('f',   '\xc')
      , ('r',   '\xd')
      , ('e',   '\x1b')
      , (' ',   ' ')
      , ('"',   '"')
      , ('/',   '/')
      , ('\\',  '\\')
      , ('N',   '\x85')
      , ('_',   '\xa0')
      , ('L',   '\x2028')
      , ('P',   '\x2029')
      ]
unescape _ = Nothing

--
-- $start
--
-- "Data.YAML" module provides us with API which allow us to interact with YAML data at the cost of some presentation details.
-- In contrast, this module provide us with API which gives us access to a other significant details like 'ScalarStyle's, 'NodeStyle's, 'Comment's, etc.
--
-- $parsing
--
-- Suppose you want to parse this YAML Document while preserving its format and comments
--
-- @
-- # Home runs
-- hr:  65
-- # Runs Batted In
-- rbi: 147
-- @
--
-- then you might want to use the function 'parseEvents'.
--
-- The following is a reference implementation of a function using 'parseEvents'.
-- It takes a YAML document as input and prints the parsed YAML 'Event's.
--
-- @
-- import Data.YAML.Event
-- import qualified Data.ByteString.Lazy as BS.L
--
-- printEvents :: BS.L.ByteString -> IO ()
-- printEvents input =
--   forM_ ('parseEvents' input) $ \ev -> case ev of
--     Left _ -> error "Failed to parse"
--     Right event -> print ('eEvent' event)
-- @
--
-- When we pass the above mentioned YAML document to the function /printEvents/ it outputs the following:
--
-- > StreamStart
-- > DocumentStart NoDirEndMarker
-- > MappingStart Nothing Nothing Block
-- > Comment " Home runs"
-- > Scalar Nothing Nothing Plain "hr"
-- > Scalar Nothing Nothing Plain "65"
-- > Comment " Runs Batted In"
-- > Scalar Nothing Nothing Plain "rbi"
-- > Scalar Nothing Nothing Plain "147"
-- > MappingEnd
-- > DocumentEnd False
-- > StreamEnd
--
-- Notice that now we have all the necessary details in the form of 'Event's.
--
-- We can now write simple functions to work with this data without losing any more details.
--
-- $serialize
--
-- Now, suppose we want to generate back the YAML document after playing with the Event-stream,
-- then you might want to use 'writeEvents'.
--
-- The following function takes a YAML document as a input and dumps it back to STDOUT after a round-trip.
--
-- @
-- import Data.YAML.Event
-- import qualified Data.YAML.Token as YT
-- import qualified Data.ByteString.Lazy as BS.L
--
-- yaml2yaml :: BS.L.ByteString -> IO ()
-- yaml2yaml input = case sequence $ parseEvents input of
--     Left _ -> error "Parsing Failure!"
--     Right events -> do
--       BS.L.hPutStr stdout (writeEvents YT.UTF8 (map eEvent events))
--       hFlush stdout
-- @
--
-- Let this be the sample document passed to the above function
--
-- @
-- # This is a 'Directives' Marker
-- ---
-- # All 'Comment's are preserved
-- date    : 2019-07-12
-- bill-to : # 'Anchor' represents a map node
--    &id001
--     address:
--         lines: # This a Block 'Scalar' with 'Keep' chomping Indicator and 'IndentAuto' Indentant indicator
--                 |+ # Extra Indentation (non-content white space) will not be preserved
--                       Vijay
--                       IIT Hyderabad
--
--
--         # Trailing newlines are a preserved here as they are a part of the 'Scalar' node
--         country    : India
-- ship-to  : # This is an 'Alias'
--            *id001
-- # Key is a 'Scalar' and Value is a Sequence
-- Other Details:
--           total: $ 3000
--           # 'Tag's are also preserved
--           Online Payment: !!bool True
--           product:
--               - Item1
--               # This comment is inside a Sequence
--               - Item2
-- ...
-- # 'DocumentEnd' True
-- # 'StreamEnd'
-- @
--
-- This function outputs the following
--
-- @
-- # This is a 'Directives' Marker
-- ---
-- # All 'Comment's are preserved
-- date: 2019-07-12
-- bill-to: # 'Anchor' represents a map node
--   &id001
--   address:
--     lines: # This a Block 'Scalar' with 'Keep' chomping Indicator and 'IndentAuto' Indentant indicator
--       # Extra Indentation (non-content white space) will not be preserved
--       |+
--       Vijay
--       IIT Hyderabad
--
--
--     # Trailing newlines are a preserved here as they are a part of the 'Scalar' node
--     country: India
-- ship-to: # This is an 'Alias'
--   *id001
-- # Key is a 'Scalar' and Value is a Sequence
-- Other Details:
--   total: $ 3000
--   # 'Tag's are also preserved
--   Online Payment: !!bool True
--   product:
--   - Item1
--   # This comment is inside a Sequence
--   - Item2
-- ...
-- # 'DocumentEnd' True
-- # 'StreamEnd'
-- @
--
-- $commenting
--
-- Round-tripping at event-level will preserve all the comments and their relative position in the YAML-document but still,
-- we lose some information like the exact indentation and the position at which the comments were present previously.
-- This information sometimes can be quite important for human-perception of comments.
-- Below are some guildlines, so that you can avoid ambiguities.
--
-- 1) Always try to start your comment in a newline. This step will avoid most of the ambiguities.
--
-- 2) Comments automaticly get indented according to the level in which they are present. For example,
--
-- Input YAML-document
--
-- @
-- # Level 0
-- - a
-- # Level 0
-- - - a
-- # Level 1
--   - a
--   - - a
-- # Level 2
--     - a
-- @
--
-- After a round-trip looks like
--
-- @
-- # Level 0
-- - a
-- # Level 0
-- - - a
--   # Level 1
--   - a
--   - - a
--     # Level 2
--     - a
-- @
--
-- 3) Comments immediately after a 'Scalar' node, start from a newline. So avoid commenting just after a scalar ends, as it may lead to some ambiguity. For example,
--
-- Input YAML-document
--
-- @
-- - scalar # After scalar
-- - random  : scalar # After scalar
--   key: 1
-- # not after scalar
-- - random  : scalar
--   key: 1
-- - random  : # not after scalar
--             scalar
--   # not after scalar
--   key: 1
-- @
--
-- After a round-trip looks like
--
-- @
-- - scalar
-- # After scalar
-- - random: scalar
--   # After scalar
--   key: 1
--   # not after scalar
-- - random: scalar
--   key: 1
-- - random: # not after scalar
--     scalar
--   # not after scalar
--   key: 1
-- @
--
-- 4) Similarly in flow-style, avoid commenting immediately after a /comma/ (@,@) seperator. Comments immediately after a /comma/ (@,@) seperator will start from a new line
--
-- Input YAML-document
--
-- @
-- {
--     # comment 0
--     Name: Vijay # comment 1
--     ,
--     # comment 2
--     age: 19, # comment 3
--     # comment 4
--     country: India # comment 5
-- }
-- @
--
-- After a round-trip looks like
--
-- @
-- {
--   # comment 0
--   Name: Vijay,
--   # comment 1
--   # comment 2
--   age: 19,
--   # comment 3
--   # comment 4
--   country: India,
--   # comment 5
-- }
-- @
--
-- 5) Avoid commenting in between syntatical elements. For example,
--
-- Input YAML-document
--
-- @
-- ? # Complex key starts
--   [
--      a,
--      b
--   ]
--  # Complex key ends
-- : # Complex Value starts
--   ? # Complex key starts
--     [
--        a,
--        b
--     ]
--     # Complex key ends
--   : # Simple value
--     a
--   # Complex value ends
-- @
--
-- After a round-trip looks like
--
-- @
-- ? # Complex key starts
--   [
--      a,
--      b
--  ]
-- : # Complex key ends
--   # Complex Value starts
--
--   ? # Complex key starts
--     [
--        a,
--        b
--    ]
--   : # Complex key ends
--     # Simple value
--     a
--   # Complex value ends
-- @
--
-- The above two YAML-documents, after parsing produce the same 'Event'-stream.
--
-- So, these are some limitation of this Format-preserving YAML processor.
