{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.YAML where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS.L
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.YAML.Event      as YE
import qualified Data.YAML.Internal   as Y

import           Data.YAML.Event      (Tag)

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

-- TODO: this is the JSON-ish semantic data-model; we need a YAML AST at some point
data Value = Object !(Map Text Value)
           | Array  [Value]
           | String !Text
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Ord,Eq)

decode :: BS.ByteString -> Maybe Value
decode bs0 = go (Y.yaml "" (BS.L.fromStrict bs0) False)
  where
    go toks = error (show toks)


testParse :: BS.ByteString -> IO ()
testParse bs0 = mapM_  print $ Y.yaml "<stdin>" (BS.L.fromStrict bs0) True

-- failsafe schema
data Node = Scalar !Tag !Text
          | Mapping !Tag (Map Node Node)
          | Sequence !Tag [Node]
          deriving (Eq,Ord,Show)


-- type P a = [YE.Event] -> Either String ([YE.Event],a)

data S n = S { sEvs :: [YE.Event]
           , sDict  :: Map YE.Anchor n
           }

type P n a = StateT (S n) (Either String) a

runParser :: P n a -> [YE.Event] -> Either String a
runParser act s0 = evalStateT act (S s0 mempty)

satisfy :: (YE.Event -> Bool) -> P n YE.Event
satisfy p = do
  s0 <- get
  case sEvs s0 of
    [] -> throwError "satisfy: premature eof"
    (ev:rest)
       | p ev -> do put (s0 { sEvs = rest})
                    return ev
       | otherwise -> throwError ("satisfy: predicate failed " ++ show ev)


peek :: P n (Maybe YE.Event)
peek = do
  s0 <- get
  case sEvs s0 of
    []     -> return Nothing
    (ev:_) -> return (Just ev)

peek1 :: P n YE.Event
peek1 = maybe (throwError "peek1: premature eof") return =<< peek

anyEv :: P n YE.Event
anyEv = satisfy (const True)

eof :: P n ()
eof = do
  s0 <- get
  case sEvs s0 of
    [] -> return ()
    _  -> throwError "eof expected"

-- NB: consumes the end-event
manyUnless :: (YE.Event -> Bool) -> P n a -> P n [a]
manyUnless pred act = do
  t0 <- peek1
  if pred t0
    then anyEv >> return []
    else liftA2 (:) act (manyUnless pred act)


data Loader nodeTy = Loader
  { yScalar   :: Tag -> YE.Style -> Text ->  Either String nodeTy
  , ySequence :: Tag -> [nodeTy] ->          Either String nodeTy
  , yMapping  :: Tag -> [(nodeTy,nodeTy)] -> Either String nodeTy
  }

failsafeLoader :: Loader Node
failsafeLoader = Loader { yScalar   = \t _ v -> Right (Scalar t v)
                        , ySequence = \t vs  -> Right (Sequence t vs)
                        , yMapping  = \t kvs -> Right (Mapping t (M.fromList kvs))
                        }

decodeLoader :: forall n . Loader n -> BS.L.ByteString -> Either String [n]
decodeLoader Loader{..} = either (\(_,err) -> Left err) (runParser goStream) . sequence . YE.parseEvents
  where
    goStream = do
      satisfy (== YE.StreamStart)
      ds <- manyUnless (== YE.StreamEnd) goDoc
      eof
      return ds

    goDoc = do
      satisfy (== YE.DocumentStart)
      modify $ \s0 -> s0 { sDict = mempty }
      n <- goNode
      satisfy (== YE.DocumentEnd)
      return n

    returnNode :: (Maybe YE.Anchor) -> Either String n -> P n n
    returnNode _ (Left err) = throwError err
    returnNode Nothing (Right node) = return node
    returnNode (Just a) (Right node) = do
      modify $ \s0 -> s0 { sDict = M.insert a node (sDict s0) }
      return node

    goNode = do
      n <- satisfy (const True)
      case n of
        YE.Scalar manc tag sty val -> do
          returnNode manc $! yScalar tag sty val
        YE.SequenceStart manc tag -> do
          ns <- manyUnless (== YE.SequenceEnd) goNode
          returnNode manc $! ySequence tag ns

        YE.MappingStart manc tag -> do
          kvs <- manyUnless (== YE.MappingEnd) (liftM2 (,) goNode goNode)
          returnNode manc $! yMapping tag kvs

        YE.Alias a -> do
          d <- gets sDict
          case M.lookup a d of
            Nothing -> throwError ("anchor not found: " ++ show a)
            Just n  -> return n

        _ -> throwError "goNode: unexpected event"


newtype Doc n = Doc n deriving (Eq,Ord,Show)

decodeNode :: BS.L.ByteString -> Either String [Doc Node]
decodeNode bs0 = map Doc <$> decodeLoader failsafeLoader bs0
