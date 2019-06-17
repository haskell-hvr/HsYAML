{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Loader
    ( decodeLoader
    , Loader(..)
    , NodeId
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Map             as Map
import qualified Data.Set             as Set

import           Data.YAML.Event      (Tag)
import qualified Data.YAML.Event      as YE
import           Util

-- | Unique identifier for identifying nodes
--
-- This is allows to observe the alias/anchor-reference structure
type NodeId = Word

-- | Structure defining how to construct a document tree/graph
--
data Loader m n = Loader
  { yScalar   :: Tag -> YE.ScalarStyle -> Text -> YE.Pos -> m (Either String n)
  , ySequence :: Tag -> [n]                    -> YE.Pos -> m (Either String n)
  , yMapping  :: Tag -> [(n,n)]                -> YE.Pos -> m (Either String n)
  , yAlias    :: NodeId -> Bool -> n           -> YE.Pos -> m (Either String n)
  , yAnchor   :: NodeId -> n                   -> YE.Pos -> m (Either String n)
  }

-- | Generalised document tree/graph construction
--
-- This doesn't yet perform any tag resolution (thus all scalars are
-- represented as 'Text' values). See also 'decodeNode' for a more
-- convenient interface.
{-# INLINEABLE decodeLoader #-}
decodeLoader :: forall n m . MonadFix m => Loader m n -> BS.L.ByteString -> m (Either (YE.Pos, String) [n])
decodeLoader Loader{..} bs0 = do
    case sequence . YE.parseEvents $ bs0 of
      Left (pos,err) -> return $ Left (pos,err)
      Right evs      -> runParserT goStream evs
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

    returnNode :: YE.Pos -> Maybe YE.Anchor -> Either (YE.Pos, String) n -> PT n m n
    returnNode _ _ (Left err) = throwError err
    returnNode _ Nothing (Right node) = return node
    returnNode pos (Just a) (Right node) = do
      nid <- getNewNid
      node0 <- lift' pos $ yAnchor nid node pos
      node' <- liftEither node0
      modify $ \s0 -> s0 { sDict = Map.insert a (nid,node') (sDict s0) }
      return node'

    registerAnchor :: YE.Pos -> Maybe YE.Anchor -> PT n m n -> PT n m n
    registerAnchor _ Nothing  pn = pn
    registerAnchor pos (Just a) pn = do
      modify $ \s0 -> s0 { sCycle = Set.insert a (sCycle s0) }
      nid <- getNewNid

      mdo
        modify $ \s0 -> s0 { sDict = Map.insert a (nid,n) (sDict s0) }
        n0 <- pn
        n1 <- lift' pos $ yAnchor nid n0 pos
        n <-  liftEither n1
        return n

    exitAnchor :: Maybe YE.Anchor -> PT n m ()
    exitAnchor Nothing = return ()
    exitAnchor (Just a) = modify $ \s0 -> s0 { sCycle = Set.delete a (sCycle s0) }

    goNode :: PT n m n
    goNode = do
      n <- anyEv
      let pos = YE.ePos n
      case YE.eEvent n of
        YE.Scalar manc tag sty val -> do
          exitAnchor manc
          n' <- lift' pos $ yScalar tag sty val pos
          returnNode pos manc $! n'

        YE.SequenceStart manc tag _ -> registerAnchor pos manc $ do
          ns <- manyUnless (== YE.SequenceEnd) goNode
          exitAnchor manc
          liftEither =<< lift' pos (ySequence tag ns pos)

        YE.MappingStart manc tag _ -> registerAnchor pos manc $ do
          kvs <- manyUnless (== YE.MappingEnd) (liftM2 (,) goNode goNode)
          exitAnchor manc
          liftEither =<< lift' pos (yMapping tag kvs pos)

        YE.Alias a -> do
          d <- gets sDict
          cy <- gets sCycle
          case Map.lookup a d of
            Nothing -> throwError (pos, ("anchor not found: " ++ show a))
            Just (nid,n') -> liftEither =<< lift' pos (yAlias nid (Set.member a cy) n' pos)

        _ -> throwError (pos, "goNode: unexpected event")

    lift' pos a = do
      n <- (lift a)
      case n of
        Left err -> return $ Left (pos, err)
        Right x -> return $ Right x



----------------------------------------------------------------------------
-- small parser framework


data S n = S { sEvs   :: [YE.EvPos]
             , sDict  :: Map YE.Anchor (Word,n)
             , sCycle :: Set YE.Anchor
             , sIdCnt :: !Word
             }

newtype PT n m a = PT (StateT (S n) (ExceptT (YE.Pos, String) m) a)
                 deriving ( Functor
                          , Applicative
                          , Monad
                          , MonadState (S n)
                          , MonadError (YE.Pos, String)
                          , MonadFix
                          )

instance MonadTrans (PT n) where
  lift = PT . lift . lift

runParserT :: Monad m => PT n m a -> [YE.EvPos] -> m (Either (YE.Pos, String) a)
runParserT (PT act) s0 = runExceptT $ evalStateT act (S s0 mempty mempty 0)

satisfy :: Monad m => (YE.Event -> Bool) -> PT n m YE.EvPos
satisfy p = do
  s0 <- get
  case sEvs s0 of
    [] -> throwError (fakePos, "satisfy: premature eof")
    (ev:rest)
       | p (YE.eEvent ev) -> do put (s0 { sEvs = rest})
                                return ev
       | otherwise        -> throwError (YE.ePos ev, ("satisfy: predicate failed " ++ show ev))

peek :: Monad m => PT n m (Maybe YE.EvPos)
peek = do
  s0 <- get
  case sEvs s0 of
    []     -> return Nothing
    (ev:_) -> return (Just ev)

peek1 :: Monad m => PT n m YE.EvPos
peek1 = maybe (throwError (fakePos,"peek1: premature eof")) return =<< peek

anyEv :: Monad m => PT n m YE.EvPos
anyEv = satisfy (const True)

eof :: Monad m => PT n m ()
eof = do
  s0 <- get
  case sEvs s0 of
    [] -> return ()
    (ev:_)  -> throwError (YE.ePos ev, "eof expected")

-- NB: consumes the end-event
manyUnless :: Monad m => (YE.Event -> Bool) -> PT n m a -> PT n m [a]
manyUnless p act = do
  t0 <- peek1
  if p (YE.eEvent t0)
    then anyEv >> return []
    else liftM2 (:) act (manyUnless p act)

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

fakePos :: YE.Pos
fakePos = YE.Pos { posByteOffset = -1 , posCharOffset = -1  , posLine = 1 , posColumn = 0 }
