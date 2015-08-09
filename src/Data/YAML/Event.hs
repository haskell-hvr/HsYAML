module Data.YAML.Event
    ( Event(..)
    , parseEvents
    ) where

import qualified Data.ByteString.Lazy as BS.L
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.YAML.Internal   as Y

-- basic libyaml event types
-- TODO: consider also non-essential attributes

{-

stream   ::= StreamStart document* StreamEnd
document ::= DocumentStart node DocumentEnd
node     ::= Alias
           | Scalar
           | sequence
           | mapping
sequence ::= SequenceStart node* SequenceEnd
mapping  ::= MappingStart (node node)* MappingEnd

-}

data Event
    = StreamStart
    | StreamEnd
    | DocumentStart
    | DocumentEnd
    | Alias          !Anchor
    | Scalar         !OptAnchor  !Tag  !Text
    | SequenceStart  !OptAnchor  !Tag
    | SequenceEnd
    | MappingStart   !OptAnchor  !Tag
    | MappingEnd
    deriving (Show, Eq)

type Anchor = Text

type OptAnchor = Maybe Anchor

type Tag = Maybe Text

type Props = (OptAnchor,Tag)

type EvStream = [Either (Int,String) Event]

parseEvents :: BS.L.ByteString -> EvStream
parseEvents = \bs0 -> Right StreamStart : (go0 $ stripComments $ filter (not . isWhite) (Y.yaml "" bs0 False))
  where
    isTCode tc = (== tc) . Y.tCode
    skipPast tc (t : ts)
      | isTCode tc t = ts
      | otherwise = skipPast tc ts
    skipPast _ [] = error "the impossible happened"

    err :: Tok2EvStream
    err (Y.Token { Y.tCode = Y.Error, Y.tByteOffset = ofs, Y.tText = msg } : _) = [Left (ofs, msg)]
    err (Y.Token { Y.tByteOffset = ofs } : _) = [Left (ofs, "")]
    err [] = [Left (-1,"Unexpected end of token stream")]

    go0 :: Tok2EvStream
    go0 [] = [Right StreamEnd]
    go0 (Y.Token { Y.tCode = Y.White } : _) = error "the impossible happened"
    go0 (Y.Token { Y.tCode = Y.Indicator } : rest) = go0 rest -- ignore indicators here
    go0 (Y.Token { Y.tCode = Y.DirectivesEnd } : rest) = go0 rest
    go0 (Y.Token { Y.tCode = Y.BeginDocument } : rest) = Right DocumentStart : go0 rest
    go0 (Y.Token { Y.tCode = Y.EndDocument } : rest) = Right DocumentEnd : go0 rest
    go0 (Y.Token { Y.tCode = Y.DocumentEnd } : rest) = go0 rest
    go0 (Y.Token { Y.tCode = Y.BeginNode } : rest) = goNode rest go0
    go0 (Y.Token { Y.tCode = Y.BeginDirective } : rest) = go0 $ skipPast Y.EndDirective rest -- TODO
    go0 xs = err xs

    goNode :: Tok2EvStreamCont
    goNode (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont = goScalar mempty mempty rest (flip goNodeEnd cont)
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
    goNode' props (Y.Token { Y.tCode = Y.BeginScalar }   : rest) cont   = goScalar props mempty rest (flip goNodeEnd cont)
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
          cont = cont (anchor,Just $! T.pack ("tag:yaml.org,2002:" ++ tag)) rest
    goTag (anchor,_) (Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "<" } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = ">" } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont = cont (anchor,Just $! T.pack tag) rest
    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = h } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont = cont (anchor,Just $! T.pack ("!" ++ h ++ "!" ++ tag)) rest -- TODO: handle lookup
    goTag (anchor,_) (Y.Token { Y.tCode = Y.BeginHandle } :
                      Y.Token { Y.tCode = Y.Indicator, Y.tText = "!" } :
                      Y.Token { Y.tCode = Y.EndHandle } :
                      Y.Token { Y.tCode = Y.Meta, Y.tText = tag } :
                      Y.Token { Y.tCode = Y.EndTag } : rest)
          cont = cont (anchor,Just $! T.pack ('!' : tag)) rest
    goTag _ xs _ = err xs

    goScalar :: Props -> String -> Tok2EvStreamCont
    goScalar props acc (Y.Token { Y.tCode = Y.Text, Y.tText = t } : rest) cont = goScalar props (acc ++ t) rest cont
    goScalar props acc (Y.Token { Y.tCode = Y.LineFold } : rest) cont = goScalar props (acc ++ " ") rest cont
    goScalar props acc (Y.Token { Y.tCode = Y.LineFeed } : rest) cont = goScalar props (acc ++ "\n") rest cont
    goScalar (manchor,tag) acc (Y.Token { Y.tCode = Y.EndScalar } : rest) cont = Right (Scalar manchor tag (T.pack acc)) : cont rest
    goScalar _props _acc xs _cont = err xs

    goSeq :: Tok2EvStreamCont
    goSeq (Y.Token { Y.tCode = Y.EndSequence } : rest) cont = Right SequenceEnd : cont rest
    goSeq (Y.Token { Y.tCode = Y.BeginNode } : rest) cont = goNode rest (flip goSeq cont)
    goSeq (Y.Token { Y.tCode = Y.Indicator } : rest) cont = goSeq rest cont
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

    isWhite :: Y.Token -> Bool
    isWhite (Y.Token { Y.tCode = Y.White })  = True
    isWhite (Y.Token { Y.tCode = Y.Indent }) = True
    isWhite (Y.Token { Y.tCode = Y.Break })  = True
    isWhite _                                = False


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
