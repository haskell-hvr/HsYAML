{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-3.0
--
-- Document oriented YAML parsing API
--
module Data.YAML
    ( decodeLoader, Loader(..), NodeId
    , decodeNode, decodeNode', Doc(..), Node(..)
    ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.ByteString.Lazy   as BS.L
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Monoid            (mempty)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Data.Word

import           Data.YAML.Event        (Tag)
import qualified Data.YAML.Event        as YE

#if !MIN_VERSION_mtl(2,2,2)
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
#endif

{-
-- TODO: this is the JSON-ish semantic data-model; we need a YAML AST at some point
data Value = Object !(Map Text Value)
           | Array  [Value]
           | String !Text
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Ord,Eq)
-}

----------------------------------------------------------------------------

data S n = S { sEvs   :: [YE.Event]
             , sDict  :: Map YE.Anchor (Word,n)
             , sCycle :: Set YE.Anchor
             , sIdCnt :: !Word
             }

newtype PT n m a = PT (StateT (S n) (ExceptT String m) a)
                 deriving ( Functor
                          , Applicative
                          , Monad
                          , MonadState (S n)
                          , MonadError String
                          , MonadFix
                          )

instance MonadTrans (PT n) where
  lift = PT . lift . lift

runParserT :: Monad m => PT n m a -> [YE.Event] -> m (Either String a)
runParserT (PT act) s0 = runExceptT $ evalStateT act (S s0 mempty mempty 0)

satisfy :: Monad m => (YE.Event -> Bool) -> PT n m YE.Event
satisfy p = do
  s0 <- get
  case sEvs s0 of
    [] -> throwError "satisfy: premature eof"
    (ev:rest)
       | p ev -> do put (s0 { sEvs = rest})
                    return ev
       | otherwise -> throwError ("satisfy: predicate failed " ++ show ev)


peek :: Monad m => PT n m (Maybe YE.Event)
peek = do
  s0 <- get
  case sEvs s0 of
    []     -> return Nothing
    (ev:_) -> return (Just ev)

peek1 :: Monad m => PT n m YE.Event
peek1 = maybe (throwError "peek1: premature eof") return =<< peek

anyEv :: Monad m => PT n m YE.Event
anyEv = satisfy (const True)

eof :: Monad m => PT n m ()
eof = do
  s0 <- get
  case sEvs s0 of
    [] -> return ()
    _  -> throwError "eof expected"

-- NB: consumes the end-event
manyUnless :: Monad m => (YE.Event -> Bool) -> PT n m a -> PT n m [a]
manyUnless p act = do
  t0 <- peek1
  if p t0
    then anyEv >> return []
    else liftM2 (:) act (manyUnless p act)


type NodeId = Word

data Loader m n = Loader
  { yScalar   :: Tag -> YE.Style -> Text -> m (Either String n)
  , ySequence :: Tag -> [n]              -> m (Either String n)
  , yMapping  :: Tag -> [(n,n)]          -> m (Either String n)
  , yAlias    :: NodeId -> Bool -> n     -> m (Either String n)
  , yAnchor   :: NodeId -> n             -> m (Either String n)
  }


{-# INLINEABLE decodeLoader #-}
decodeLoader :: forall n m . MonadFix m => Loader m n -> BS.L.ByteString -> m (Either String [n])
decodeLoader Loader{..} bs0 = do
    case sequence . YE.parseEvents $ bs0 of
      Left (_,err) -> return (Left err)
      Right evs    -> runParserT goStream evs
  where
    goStream :: PT n m [n]
    goStream = do
      _ <- satisfy (== YE.StreamStart)
      ds <- manyUnless (== YE.StreamEnd) goDoc
      eof
      return ds

    goDoc :: PT n m n
    goDoc = do
      _ <- satisfy isDocStart
      modify $ \s0 -> s0 { sDict = mempty, sCycle = mempty }
      n <- goNode
      _ <- satisfy isDocEnd
      return n

    getNewNid :: PT n m Word
    getNewNid = state $ \s0 -> let i0 = sIdCnt s0
                               in (i0, s0 { sIdCnt = i0+1 })

    returnNode :: (Maybe YE.Anchor) -> Either String n -> PT n m n
    returnNode _ (Left err) = throwError err
    returnNode Nothing (Right node) = return node
    returnNode (Just a) (Right node) = do
      nid <- getNewNid
      node0 <- lift $ yAnchor nid node
      node' <- liftEither node0
      modify $ \s0 -> s0 { sDict = Map.insert a (nid,node') (sDict s0) }
      return node'

    registerAnchor :: Maybe YE.Anchor -> PT n m n -> PT n m n
    registerAnchor Nothing  pn = pn
    registerAnchor (Just a) pn = do
      modify $ \s0 -> s0 { sCycle = Set.insert a (sCycle s0) }
      nid <- getNewNid

      mdo
        modify $ \s0 -> s0 { sDict = Map.insert a (nid,n) (sDict s0) }
        n0 <- pn
        n1 <- lift $ yAnchor nid n0
        n <-  liftEither n1
        return n

    exitAnchor :: Maybe YE.Anchor -> PT n m ()
    exitAnchor Nothing = return ()
    exitAnchor (Just a) = modify $ \s0 -> s0 { sCycle = Set.delete a (sCycle s0) }

    goNode :: PT n m n
    goNode = do
      n <- satisfy (const True)
      case n of
        YE.Scalar manc tag sty val -> do
          exitAnchor manc
          n' <- lift $ yScalar tag sty val
          returnNode manc $! n'

        YE.SequenceStart manc tag -> registerAnchor manc $ do
          ns <- manyUnless (== YE.SequenceEnd) goNode
          exitAnchor manc
          liftEither =<< (lift $ ySequence tag ns)

        YE.MappingStart manc tag -> registerAnchor manc $ do
          kvs <- manyUnless (== YE.MappingEnd) (liftM2 (,) goNode goNode)
          exitAnchor manc
          liftEither =<< (lift $ yMapping tag kvs)

        YE.Alias a -> do
          d <- gets sDict
          cy <- gets sCycle
          case Map.lookup a d of
            Nothing -> throwError ("anchor not found: " ++ show a)
            Just (nid,n') -> liftEither =<< (lift $ yAlias nid (Set.member a cy) n')

        _ -> throwError "goNode: unexpected event"


newtype Doc n = Doc n deriving (Eq,Ord,Show)

-- failsafe schema
data Node = Scalar !Tag !Text
          | Mapping !Tag (Map Node Node)
          | Sequence !Tag [Node]
          | Anchor !NodeId !Node
          deriving (Eq,Ord,Show)

decodeNode :: BS.L.ByteString -> Either String [Doc Node]
decodeNode bs0 = map Doc <$> runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t _ v -> pure $ Right (Scalar t v)
                            , ySequence = \t vs  -> pure $ Right (Sequence t vs)
                            , yMapping  = \t kvs -> pure $ Right (Mapping t (Map.fromList kvs))
                            , yAlias    = \_ _ n -> pure $ Right n
                            , yAnchor   = \j n   -> pure $ Right (Anchor j n)
                            }

decodeNode' :: BS.L.ByteString -> Either String [Doc Node]
decodeNode' bs0 = map Doc <$> runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t _ v -> pure $ Right (Scalar t v)
                            , ySequence = \t vs  -> pure $ Right (Sequence t vs)
                            , yMapping  = \t kvs -> pure $ Right (Mapping t (Map.fromList kvs))
                            , yAlias    = \_ c n -> pure $ if c then Left "cycle detected" else Right n
                            , yAnchor   = \_ n   -> pure $ Right n
                            }

{-
tryError :: MonadError e m => m a -> m (Either e a)
tryError act = catchError (Right <$> act) (pure . Left)
-}

isDocStart :: YE.Event -> Bool
isDocStart (YE.DocumentStart _) = True
isDocStart _                    = False

isDocEnd :: YE.Event -> Bool
isDocEnd (YE.DocumentEnd _) = True
isDocEnd _                  = False
