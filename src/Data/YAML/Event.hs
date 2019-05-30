{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Event-stream oriented YAML parsing API
module Data.YAML.Event
    ( parseEvents
    , writeEvents
    , writeEventsText
    , EvStream
    , Event(..)
    , Directives(..)
    , ScalarStyle(..)
    , NodeStyle(..)
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


tok2pos :: Y.Token -> Pos
tok2pos Y.Token { Y.tByteOffset = posByteOffset, Y.tCharOffset = posCharOffset, Y.tLine = posLine, Y.tLineChar = posColumn } = Pos {..}

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

-- | Parse YAML 'Event's from a lazy 'BS.L.ByteString'.
--
-- The input 'BS.L.ByteString' is expected to have a YAML 1.2 stream
-- using the UTF-8, UTF-16 (LE or BE), or UTF-32 (LE or BE) encodings
-- (which will be auto-detected).
parseEvents :: BS.L.ByteString -> EvStream
parseEvents = \bs0 -> Right StreamStart : (go0 $ stripComments $ filter (not . isWhite) $ Y.tokenize bs0 False)
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
    go0 []                                                = [Right StreamEnd]
    go0 toks0@(Y.Token { Y.tCode = Y.BeginDocument } : _) = go1 dinfo0 toks0
    go0 (Y.Token { Y.tCode = Y.DocumentEnd } : rest)      = go0 rest -- stray/redundant document-end markers cause this
    go0 xs                                                = err xs


    go1 :: DInfo -> Tok2EvStream
    go1 m (Y.Token { Y.tCode = Y.BeginDocument } : rest) = goDirs m rest
    go1 _ (Y.Token { Y.tCode = Y.EndDocument } : Y.Token { Y.tCode = Y.DocumentEnd } : rest) = Right (DocumentEnd True) : go0 rest
    go1 _ (Y.Token { Y.tCode = Y.EndDocument } : rest) = Right (DocumentEnd False) : go0 rest
    go1 m (Y.Token { Y.tCode = Y.BeginNode } : rest) = goNode0 m rest (go1 m)
    go1 _ xs = err xs

    -- consume {Begin,End}Directives and emit DocumentStart event
    goDirs :: DInfo -> Tok2EvStream
    goDirs m (Y.Token { Y.tCode = Y.BeginDirective } : rest) = goDir1 m rest
    goDirs m (Y.Token { Y.tCode = Y.DirectivesEnd } : rest)
      | Just (1,mi) <- diVer m = Right (DocumentStart (DirEndMarkerVersion mi)) : go1 m rest
      | otherwise              = Right (DocumentStart DirEndMarkerNoVersion) : go1 m rest
    goDirs _ xs@(Y.Token { Y.tCode = Y.BeginDocument } : _) = err xs
    goDirs m xs = Right (DocumentStart NoDirEndMarker) : go1 m xs

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
    goNode (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont = goScalar (mempty,untagged) rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginSequence } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (SequenceStart Nothing untagged (seqInd ind)) : goSeq rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginMapping }  : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (MappingStart Nothing untagged (mapInd ind)) : goMap rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginMapping }  : rest) cont = Right (MappingStart Nothing untagged Block) : goMap rest (flip goNodeEnd cont)
    goNode (Y.Token { Y.tCode = Y.BeginProperties } : rest) cont = goProp (mempty,untagged) rest (\p rest' -> goNode' p rest' cont)
    goNode (Y.Token { Y.tCode = Y.BeginAlias } :
            Y.Token { Y.tCode = Y.Indicator } :
            Y.Token { Y.tCode = Y.Meta, Y.tText = anchor } :
            Y.Token { Y.tCode = Y.EndAlias } :
            Y.Token { Y.tCode = Y.EndNode } :
            rest) cont = Right (Alias (T.pack anchor)) : cont rest
    goNode xs _cont = err xs

    goNode' :: Props -> Tok2EvStreamCont
    goNode' props (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont   = goScalar props rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (Y.Token { Y.tCode = Y.BeginSequence } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (SequenceStart manchor mtag (seqInd ind)) : goSeq rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (Y.Token { Y.tCode = Y.BeginMapping }  : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest) cont = Right (MappingStart manchor mtag (mapInd ind)) : goMap rest (flip goNodeEnd cont)
    goNode' (manchor,mtag) (Y.Token { Y.tCode = Y.BeginMapping } : rest) cont = Right (MappingStart manchor mtag Block) : goMap rest (flip goNodeEnd cont)
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

    goScalar :: Props -> Tok2EvStreamCont
    goScalar (manchor,tag) toks0 cont = go0 False Plain toks0
      where
        go0 ii sty (Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } : rest)
          | "'"  <- ind = go' ii "" SingleQuoted rest
          | "\"" <- ind = go' ii "" DoubleQuoted rest
          | "|"  <- ind = go0 True (Literal Clip (toEnum 0)) rest
          | ">"  <- ind = go0 True (Folded Clip (toEnum 0)) rest

          | "+"  <- ind = go0 ii (chn sty Keep) rest
          | "-"  <- ind = go0 ii (chn sty Strip) rest
          | [c]  <- ind, '1' <= c, c <= '9' = go0 False (chn' sty (C.digitToInt c)) rest

        go0 ii sty (Y.Token { Y.tCode = Y.Text, Y.tText = t } : rest)      = go' ii t sty rest
        go0 ii sty (Y.Token { Y.tCode = Y.LineFold } : rest)               = go' ii " " sty rest
        go0 ii sty (Y.Token { Y.tCode = Y.LineFeed } : rest)               = go' ii "\n" sty rest
        go0 _  sty (Y.Token { Y.tCode = Y.EndScalar } : rest)              = Right (Scalar manchor tag sty mempty) : cont rest

        go0 _ _ xs = err xs

        chn :: ScalarStyle -> Chomp -> ScalarStyle
        chn (Literal _ digit) chmp = Literal chmp digit
        chn (Folded _ digit) chmp = Folded chmp digit
        chn _ _ = error "impossible"

        chn' :: ScalarStyle -> Int -> ScalarStyle
        chn' (Literal b _) digit = Literal b (toEnum digit)
        chn' (Folded b _) digit = Folded b (toEnum digit)
        chn' _ _ = error "impossible"

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
          | otherwise = Right (Scalar manchor tag sty (T.pack acc)) : cont rest

        go' _ _ _ xs | False = error (show xs)
        go' _ _ _ xs = err xs

        hasLeadingSpace (' ':_)   = True
        hasLeadingSpace ('\n':cs) = hasLeadingSpace cs
        hasLeadingSpace _         = False

    goSeq :: Tok2EvStreamCont
    goSeq (Y.Token { Y.tCode = Y.EndSequence } : rest) cont = Right SequenceEnd : cont rest
    goSeq (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.BeginMapping } : Y.Token { Y.tCode = Y.Indicator, Y.tText = ind } :  rest) cont = Right (MappingStart Nothing untagged (mapInd ind)) : goMap rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.BeginMapping } : rest) cont = Right (MappingStart Nothing untagged Block) : goMap rest (flip goSeq cont)
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

