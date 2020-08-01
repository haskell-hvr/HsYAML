{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}


-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Event-stream oriented YAML writer API
--
module Data.YAML.Event.Writer
    ( writeEvents
    , writeEventsText
    ) where

import           Data.YAML.Event.Internal

import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.Char                as C
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import           Text.Printf              (printf)

import qualified Data.Text.Lazy           as T.L
import qualified Data.Text.Lazy.Builder   as T.B
import qualified Data.Text.Lazy.Encoding  as T.L

import           Util


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

-- | Serialise 'Event's using specified UTF encoding to a lazy 'BS.L.ByteString'
--
-- __NOTE__: This function is only well-defined for valid 'Event' streams
--
-- @since 0.2.0.0
writeEvents :: Encoding -> [Event] -> BS.L.ByteString
writeEvents UTF8    = T.L.encodeUtf8    . writeEventsText
writeEvents UTF16LE = T.L.encodeUtf16LE . T.L.cons '\xfeff' . writeEventsText
writeEvents UTF16BE = T.L.encodeUtf16BE . T.L.cons '\xfeff' . writeEventsText
writeEvents UTF32LE = T.L.encodeUtf32LE . T.L.cons '\xfeff' . writeEventsText
writeEvents UTF32BE = T.L.encodeUtf32BE . T.L.cons '\xfeff' . writeEventsText

-- | Serialise 'Event's to lazy 'T.L.Text'
--
-- __NOTE__: This function is only well-defined for valid 'Event' streams
--
-- @since 0.2.0.0
writeEventsText :: [Event] -> T.L.Text
writeEventsText [] = mempty
writeEventsText (StreamStart:xs) = T.B.toLazyText $ goStream xs (error "writeEvents: internal error")
  where
    -- goStream :: [Event] -> [Event] -> T.B.Builder
    goStream [StreamEnd] _ = mempty
    goStream (StreamEnd : _ : _ ) _cont = error "writeEvents: events after StreamEnd"
    goStream (Comment com: rest) cont = goComment (0 :: Int) True BlockIn com (goStream rest cont)
    goStream (DocumentStart marker : rest) cont
      = case marker of
          NoDirEndMarker         -> putNode False rest (\zs -> goDoc zs cont)
          DirEndMarkerNoVersion  -> "---" <> putNode True rest (\zs -> goDoc zs cont)
          DirEndMarkerVersion mi -> "%YAML 1." <> (T.B.fromString (show mi)) <> "\n---" <> putNode True rest (\zs -> goDoc zs cont)
    goStream (x:_) _cont = error ("writeEvents: unexpected " ++ show x ++ " (expected DocumentStart or StreamEnd)")
    goStream [] _cont = error ("writeEvents: unexpected end of stream (expected DocumentStart or StreamEnd)")

    goDoc (DocumentEnd marker : rest) cont
      = (if marker then "...\n" else mempty) <> goStream rest cont
    goDoc (Comment com: rest) cont = goComment (0 :: Int) True BlockIn com (goDoc rest cont)
    goDoc ys _ = error (show ys)

    -- unexpected s l = error ("writeEvents: unexpected " ++ show l ++ " " ++ show s)

writeEventsText (x:_) = error ("writeEvents: unexpected " ++ show x ++ " (expected StreamStart)")

-- | Production context -- copied from Data.YAML.Token
data Context = BlockOut     -- ^ Outside block sequence.
             | BlockIn      -- ^ Inside block sequence.
             | BlockKey     -- ^ Implicit block key.
             | FlowOut      -- ^ Outside flow collection.
             | FlowIn       -- ^ Inside flow collection.
             | FlowKey      -- ^ Implicit flow key.
             deriving (Eq,Show)

goComment :: Int -> Bool -> Context -> T.Text -> T.B.Builder -> T.B.Builder
goComment !n !sol c comment cont = doSol <> "#" <> (T.B.fromText comment) <> doEol <> doIndent <> cont
  where
    doEol
      | not sol && n == 0  = mempty           -- "--- " case
      | sol && FlowIn == c = mempty
      | otherwise = eol

    doSol
      | not sol && (BlockOut == c || FlowOut == c) = ws
      | sol = mkInd n'
      | otherwise = eol <> mkInd n'

    n'
      | BlockOut <- c = max 0 (n - 1)
      | FlowOut  <- c = n + 1
      | otherwise     = n

    doIndent
      | BlockOut <- c = mkInd n'
      | FlowOut  <- c = mkInd n'
      | otherwise = mempty

putNode :: Bool -> [Event] -> ([Event] -> T.B.Builder) -> T.B.Builder
putNode = \docMarker -> go (-1 :: Int) (not docMarker) BlockIn
  where

    {-  s-l+block-node(n,c)

        [196]   s-l+block-node(n,c)        ::=     s-l+block-in-block(n,c) | s-l+flow-in-block(n)

        [197]   s-l+flow-in-block(n)       ::=     s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments

        [198]   s-l+block-in-block(n,c)    ::=     s-l+block-scalar(n,c) | s-l+block-collection(n,c)

        [199]   s-l+block-scalar(n,c)      ::=     s-separate(n+1,c) ( c-ns-properties(n+1,c) s-separate(n+1,c) )?  ( c-l+literal(n) | c-l+folded(n) )

        [200]   s-l+block-collection(n,c)  ::=     ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments
                                                   ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )

        [201]   seq-spaces(n,c)            ::=     c = block-out ⇒ n-1
                                                   c = block-in  ⇒ n

    -}

    go :: Int -> Bool -> Context -> [Event] -> ([Event] -> T.B.Builder) -> T.B.Builder
    go _  _ _  [] _cont = error ("putNode: expected node-start event instead of end-of-stream")
    go !n !sol c (t : rest) cont = case t of
        Scalar        anc tag sty t' -> goStr (n+1) sol c anc tag sty t' (cont rest)
        SequenceStart anc tag sty    -> goSeq (n+1) sol (chn sty) anc tag sty rest cont
        MappingStart  anc tag sty    -> goMap (n+1) sol (chn sty) anc tag sty rest cont
        Alias a                      -> pfx <> goAlias c a (cont rest)
        Comment com                  -> goComment (n+1) sol c com (go n sol c rest cont)
        EmptyLine                    -> "\n" <> go n sol c rest cont
        _ -> error ("putNode: expected node-start event instead of " ++ show t)

      where
        pfx | sol           = mempty
            | BlockKey <- c = mempty
            | FlowKey  <- c = mempty
            | otherwise     = T.B.singleton ' '

        chn sty
          | Flow <-sty, (BlockIn == c || BlockOut == c) = FlowOut
          | otherwise = c


    goMap _ sol _ anc tag _ (MappingEnd : rest) cont = pfx $ "{}\n" <> cont rest
      where
        pfx cont' = wsSol sol <> anchorTag'' (Right ws) anc tag cont'

    goMap n sol c anc tag Block xs cont = case c of
        BlockIn | not (not sol && n == 0) -- avoid "--- " case
           ->  wsSol sol <> anchorTag'' (Right (eol <> mkInd n)) anc tag
               (putKey xs putValue')
        _  ->  anchorTag'' (Left ws) anc tag $ doEol <> g' xs
      where
        g' (MappingEnd : rest) = cont rest                    -- All comments should be part of the key
        g' ys                  = pfx <> putKey ys putValue'

        g (EmptyLine : rest)  = "\n" <> g rest
        g (Comment com: rest) = goComment n True c' com (g rest)  -- For trailing comments
        g (MappingEnd : rest) = cont rest
        g ys                  = pfx <> putKey ys putValue'

        pfx = if c == BlockIn || c == BlockOut || c == BlockKey then mkInd n else ws
        c' = if FlowIn == c then FlowKey else BlockKey

        doEol = case c of
          FlowKey -> mempty
          FlowIn  -> mempty
          _       -> eol

        putKey zs cont2
          | isSmallKey zs = go n (n == 0) c' zs (\ys -> ":" <> cont2 ys)
          | Comment com: rest <- zs = "?" <> ws <> goComment 0 True BlockIn com (f rest cont2)
          | otherwise     = "?" <> go n False BlockIn zs (putValue cont2)

        f (Comment com: rest) cont2 = goComment (n + 1) True BlockIn com (f rest cont2)   -- Comments should not change position in key
        f zs cont2                  = ws <> mkInd n <> go n False BlockIn zs (putValue cont2)

        putValue cont2 zs
          | FlowIn <- c   = ws <> mkInd (n - 1) <> ":" <> cont2 zs
          | otherwise     = mkInd n <> ":" <> cont2 zs

        putValue' (Comment com: rest) = goComment (n + 1) False BlockOut com (ws <> putValue' rest) -- Comments should not change position in value
        putValue' zs = go n False (if FlowIn == c then FlowIn else BlockOut) zs g

    goMap n sol c anc tag Flow xs cont =
        wsSol sol <> anchorTag'' (Right ws) anc tag ("{" <> f xs)
          where
            f (Comment com: rest) = eol <> wsSol sol <> goComment n' True (inFlow c) com (f rest)
            f (MappingEnd : rest) = eol <> wsSol sol <> mkInd (n - 1) <> "}" <> doEol <> cont rest
            f ys                  = eol <> mkInd n' <> putKey ys putValue'

            n' = n + 1

            doEol = case c of
              FlowKey -> mempty
              FlowIn  -> mempty
              _       -> eol

            g (Comment com: rest) = "," <> eol <> wsSol sol <> goComment n' True (inFlow c) com (f rest)
            g (MappingEnd : rest) = eol <> wsSol sol <> mkInd (n - 1) <> "}" <> doEol <> cont rest
            g ys                  = "," <> eol <> mkInd n' <> putKey ys putValue'

            putKey zs cont2
              | (Comment com: rest) <- zs = goComment n' True c com (eol <> mkInd n' <> putKey rest cont2)
              | isSmallKey zs =    go n' (n == 0) FlowKey zs (if isComEv zs then putValue cont2 else (\ys -> ":" <> cont2 ys))
              | otherwise     = "?" <> go n False FlowIn zs (putValue cont2)

            putValue cont2 zs
              | Comment com: rest <- zs =  eol <> wsSol sol <> goComment n' True (inFlow c) com (putValue cont2 rest)
              | otherwise     = eol <> mkInd n' <> ":" <> cont2 zs

            putValue' zs
              | Comment com : rest <- zs = goComment n' False FlowOut com (putValue' rest)
              | otherwise = go n' False FlowIn zs g


    goSeq _ sol _ anc tag _ (SequenceEnd : rest) cont = pfx $ "[]\n" <> cont rest
      where
        pfx cont' = wsSol sol <> anchorTag'' (Right ws) anc tag cont'

    goSeq n sol c anc tag Block xs cont = case c of
        BlockOut -> anchorTag'' (Left ws) anc tag (eol <> if isComEv xs then "-" <> eol <> f xs else g xs)

        BlockIn
          | not sol && n == 0 {- "---" case -} -> goSeq n sol BlockOut anc tag Block xs cont
          | Comment com: rest <- xs ->  wsSol sol <> anchorTag'' (Right (eol <> mkInd n')) anc tag ("-" <> ws <> goComment 0 True BlockIn com (f rest))
          | otherwise -> wsSol sol <> anchorTag'' (Right (eol <> mkInd n')) anc tag ("-" <> go n' False BlockIn xs g)

        BlockKey -> error "sequence in block-key context not supported"

        _ -> error "Invalid Context in Block style"

      where
        n' | BlockOut <- c = max 0 (n - 1)
           | otherwise     = n

        g (Comment com: rest)  = goComment n' True BlockIn com (g rest)
        g (SequenceEnd : rest) = cont rest
        g ys                   = mkInd n' <> "-" <> go n' False BlockIn ys g

        f (Comment com: rest)  = goComment n' True BlockIn com (f rest)
        f (SequenceEnd : rest) = cont rest
        f ys                   = ws <> mkInd n' <> go n' False BlockIn ys g

    goSeq n sol c anc tag Flow xs cont =
      wsSol sol <> anchorTag'' (Right ws) anc tag ("[" <> f xs)
        where
          f (Comment com: rest)  = eol <> wsSol sol <> goComment n' True (inFlow c) com (f rest)
          f (SequenceEnd : rest) = eol <> wsSol sol <> mkInd (n - 1) <> "]" <> doEol <> cont rest
          f ys                   = eol <> mkInd n' <> go n' False (inFlow c) ys g

          n' = n + 1

          doEol = case c of
            FlowKey -> mempty
            FlowIn  -> mempty
            _       -> eol

          g (Comment com: rest)  = "," <> eol <> wsSol sol <> goComment n' True (inFlow c) com (f rest)
          g (SequenceEnd : rest) = eol <> wsSol sol <> mkInd (n - 1) <> "]" <> doEol <> cont rest
          g ys                   = "," <> eol <> mkInd n' <> go n' False (inFlow c) ys g


    goAlias c a cont = T.B.singleton '*' <> T.B.fromText a <> sep <> cont
      where
        sep = case c of
          BlockIn  -> eol
          BlockOut -> eol
          BlockKey -> T.B.singleton ' '
          FlowIn   -> mempty
          FlowOut  -> eol
          FlowKey  -> T.B.singleton ' '

    goStr :: Int -> Bool -> Context -> Maybe Anchor -> Tag -> ScalarStyle -> Text -> T.B.Builder -> T.B.Builder
    goStr !n !sol c anc tag sty t cont = case sty of
      -- flow-style

      Plain -- empty scalars
        | t == "" -> case () of
                      _ | Nothing <- anc, Tag Nothing <- tag  -> contEol -- not even node properties
                        | sol                                 -> anchorTag0 anc tag (if c == BlockKey || c == FlowKey then ws <> cont else contEol)
                        | BlockKey <- c                       -> anchorTag0 anc tag (ws <> cont)
                        | FlowKey <- c                        -> anchorTag0 anc tag (ws <> cont)
                        | otherwise                           -> anchorTag'' (Left ws) anc tag contEol

      Plain           -> pfx $
                          let h []     = contEol
                              h (x:xs) = T.B.fromText x <> f' xs
                                where
                                  f' []     = contEol
                                  f' (y:ys) = eol <> mkInd (n+1) <> T.B.fromText y <> f' ys
                          in h (insFoldNls (T.lines t)) -- FIXME: unquoted plain-strings can't handle leading/trailing whitespace properly

      SingleQuoted    -> pfx $ T.B.singleton '\'' <> f (insFoldNls $ T.lines (T.replace "'" "''" t) ++ [ mempty | T.isSuffixOf "\n" t]) (T.B.singleton '\'' <> contEol) -- FIXME: leading white-space (i.e. SPC) before/after LF

      DoubleQuoted    -> pfx $ T.B.singleton '"'  <> T.B.fromText (escapeDQ t) <> T.B.singleton '"'  <> contEol

      -- block style
      Folded chm iden -> pfx $ ">" <> goChomp chm <> goDigit iden <> g (insFoldNls' $ T.lines t) (fromEnum iden) cont

      Literal chm iden -> pfx $ "|" <> goChomp chm <> goDigit iden <> g (T.lines t) (fromEnum iden) cont

      where
        goDigit :: IndentOfs -> T.B.Builder
        goDigit iden = let ch = C.intToDigit.fromEnum $ iden
                       in if(ch == '0') then mempty else T.B.singleton ch

        goChomp :: Chomp -> T.B.Builder
        goChomp chm = case chm of
           Strip -> T.B.singleton '-'
           Clip  -> mempty
           Keep  -> T.B.singleton '+'

        pfx cont' = (if sol || c == BlockKey || c == FlowKey then mempty else ws) <> anchorTag'' (Right ws) anc tag cont'

        doEol = case c of
          BlockKey -> False
          FlowKey  -> False
          FlowIn   -> False
          _        -> True

        contEol
          | doEol     = eol <> cont
          | otherwise = cont

        g []     _ cont' = eol <> cont'
        g (x:xs) dig cont'
          | T.null x   = eol <> g xs dig cont'
          | dig == 0   = eol <> (if n > 0 then mkInd n else mkInd' 1) <> T.B.fromText x <> g xs dig cont'
          | otherwise  = eol <> mkInd (n-1) <> mkInd' dig <> T.B.fromText x <> g xs dig cont'

        g' []     cont' = cont'
        g' (x:xs) cont' = eol <> mkInd (n+1) <> T.B.fromText x <> g' xs cont'

        f []     cont' = cont'
        f (x:xs) cont' = T.B.fromText x <> g' xs cont'


    isSmallKey (Alias _ : _)                   = True
    isSmallKey (Scalar _ _ (Folded _ _) _: _)  = False
    isSmallKey (Scalar _ _ (Literal _ _) _: _) = False
    isSmallKey (Scalar _ _ _ _ : _)            = True
    isSmallKey (SequenceStart _ _ _ : _)       = False
    isSmallKey (MappingStart _ _ _ : _)        = False
    isSmallKey _                               = False

    -- <https://yaml.org/spec/1.2/spec.html#in-flow(c) in-flow(c)>
    inFlow c = case c of
      FlowIn   -> FlowIn
      FlowOut  -> FlowIn
      BlockKey -> FlowKey
      FlowKey  -> FlowKey
      _        -> error "Invalid context in Flow style"


    putTag t cont
      | Just t' <- T.stripPrefix "tag:yaml.org,2002:" t = "!!" <> T.B.fromText t' <> cont
      | "!" `T.isPrefixOf` t = T.B.fromText t <> cont
      | otherwise            = "!<" <> T.B.fromText t <> T.B.singleton '>' <> cont

    anchorTag'' :: Either T.B.Builder T.B.Builder -> Maybe Anchor -> Tag -> T.B.Builder -> T.B.Builder
    anchorTag'' _ Nothing (Tag Nothing) cont = cont
    anchorTag'' (Right pad) Nothing (Tag (Just t)) cont  = putTag t (pad <> cont)
    anchorTag'' (Right pad) (Just a) (Tag Nothing) cont  = T.B.singleton '&' <> T.B.fromText a <> pad <> cont
    anchorTag'' (Right pad) (Just a) (Tag (Just t)) cont = T.B.singleton '&' <> T.B.fromText a <> T.B.singleton ' ' <> putTag t (pad <> cont)
    anchorTag'' (Left pad)  Nothing (Tag (Just t)) cont  = pad <> putTag t cont
    anchorTag'' (Left pad)  (Just a) (Tag Nothing) cont  = pad <> T.B.singleton '&' <> T.B.fromText a <> cont
    anchorTag'' (Left pad)  (Just a) (Tag (Just t)) cont = pad <> T.B.singleton '&' <> T.B.fromText a <> T.B.singleton ' ' <> putTag t cont

    anchorTag0 = anchorTag'' (Left mempty)
    -- anchorTag  = anchorTag'' (Right (T.B.singleton ' '))
    -- anchorTag' = anchorTag'' (Left (T.B.singleton ' '))

isComEv :: [Event] -> Bool
isComEv (Comment _: _) = True
isComEv _              = False

-- indentation helper
mkInd :: Int -> T.B.Builder
mkInd (-1) = mempty
mkInd 0    = mempty
mkInd 1 = "  "
mkInd 2 = "    "
mkInd 3 = "      "
mkInd 4 = "        "
mkInd l
  | l < 0     = error (show l)
  | otherwise = T.B.fromText (T.replicate l "  ")

mkInd' :: Int -> T.B.Builder
mkInd' 1 = " "
mkInd' 2 = "  "
mkInd' 3 = "   "
mkInd' 4 = "    "
mkInd' 5 = "     "
mkInd' 6 = "      "
mkInd' 7 = "       "
mkInd' 8 = "        "
mkInd' 9 = "         "
mkInd' l = error ("Impossible Indentation-level" ++ show l)

eol, ws:: T.B.Builder
eol = T.B.singleton '\n'
ws  = T.B.singleton ' '

wsSol :: Bool -> T.B.Builder
wsSol sol = if sol then mempty else ws

escapeDQ :: Text -> Text
escapeDQ t
  | T.all (\c -> C.isPrint c && c /= '\\' && c /= '"') t = t
  | otherwise = T.concatMap escapeChar t

escapeChar :: Char -> Text
escapeChar c
  | c == '\\'   = "\\\\"
  | c == '"'    = "\\\""
  | C.isPrint c = T.singleton c
  | Just e <- Map.lookup c emap = e
  | x <= 0xff   = T.pack (printf "\\x%02x" x)
  | x <= 0xffff = T.pack (printf "\\u%04x" x)
  | otherwise   = T.pack (printf "\\U%08x" x)
  where
    x = ord c

    emap = Map.fromList [ (v,T.pack ['\\',k]) | (k,v) <- escapes ]


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


-- flow style line folding
-- FIXME: check single-quoted strings with leading '\n' or trailing '\n's
insFoldNls :: [Text] -> [Text]
insFoldNls [] = []
insFoldNls z0@(z:zs)
  | all T.null z0     = "" : z0 -- HACK
  | otherwise         = z : go zs
  where
    go [] = []
    go (l:ls)
      | T.null l = l : go'  ls
      | otherwise = "" : l : go  ls

    go' [] = [""]
    go' (l:ls)
      | T.null l = l : go' ls
      | otherwise = "" : l : go  ls

{- block style line folding

The combined effect of the block line folding rules is that each
“paragraph” is interpreted as a line, empty lines are interpreted as a
line feed, and the formatting of more-indented lines is preserved.

-}
insFoldNls' :: [Text] -> [Text]
insFoldNls' = go'
  where
    go []                  = []
    go (l:ls)
      | T.null l           = l : go  ls
      | isWhite (T.head l) = l : go' ls
      | otherwise          = "" : l : go  ls

    go' []                 = []
    go' (l:ls)
      | T.null l           = l : go' ls
      | isWhite (T.head l) = l : go' ls
      | otherwise          = l : go ls

    -- @s-white@
    isWhite :: Char -> Bool
    isWhite ' '  = True
    isWhite '\t' = True
    isWhite _    = False
