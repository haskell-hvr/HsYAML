{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Dumper
    ( encodeNode
    , encodeNode'
    ) where

import           Data.YAML.Event.Internal  as YE
import           Data.YAML.Event.Writer    (writeEvents)
import           Data.YAML.Internal        as YI
import           Data.YAML.Schema.Internal as YS

import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.Map                  as Map
import qualified Data.Text                 as T

-- internal
type EvList = [Either String Event]
type Node2EvList = [Node ()] -> EvList

-- | Dump YAML Nodes as a lazy 'UTF8' encoded 'BS.L.ByteString'
--
-- Each YAML 'Node' is emitted as a individual YAML Document where each Document is terminated by a 'DocumentEnd' indicator.
--
-- This is a convenience wrapper over `encodeNode'`
--
-- @since 0.2.0
encodeNode :: [Doc (Node ())] -> BS.L.ByteString
encodeNode = encodeNode' coreSchemaEncoder UTF8

-- | Customizable variant of 'encodeNode'
--
-- __NOTE__: A leading <https://en.wikipedia.org/wiki/Byte_order_mark BOM> will be emitted for all encodings /other than/ 'UTF8'.
--
-- @since 0.2.0
encodeNode' :: SchemaEncoder -> Encoding -> [Doc (Node ())] -> BS.L.ByteString
encodeNode' SchemaEncoder{..} encoding nodes = writeEvents encoding $ map getEvent (dumpEvents (map docRoot nodes))
  where

    getEvent :: Either String Event -> Event
    getEvent = \x -> case x of
      Right ev -> ev
      Left str -> error str

    dumpEvents :: Node2EvList
    dumpEvents nodes' = Right StreamStart: go0 nodes'
      where
        go0 :: [Node ()] -> EvList
        go0 [] = [Right StreamEnd]
        go0 n  = Right (DocumentStart NoDirEndMarker): goNode (0 :: Int) n (\ev -> go0 ev)


        goNode :: Int -> [Node ()] -> Node2EvList -> EvList
        goNode _ [] _ = [Left "Dumper: unexpected pattern in goNode"]
        goNode lvl (node: rest) cont = case node of
          YI.Scalar _ scalar -> goScalar scalar Nothing: isDocEnd lvl rest cont
          Mapping   _ tag m  -> Right (MappingStart Nothing (getTag schemaEncoderMapping tag) Block) : goMap (lvl + 1) m rest cont
          Sequence  _ tag s  -> Right (SequenceStart Nothing (getTag schemaEncoderSequence tag) Block) : goSeq (lvl + 1) s rest cont
          Anchor    _ nid n  -> goAnchor lvl nid n rest cont

        goScalar :: YS.Scalar -> Maybe Anchor -> Either String Event
        goScalar s anc = case schemaEncoderScalar s of
            Right (t, sty, text) -> Right (YE.Scalar anc t sty text)
            Left err             -> Left err

        goMap :: Int -> Mapping () -> [Node ()] -> Node2EvList -> EvList
        goMap lvl m rest cont = case (mapToList m) of
          []   -> Right MappingEnd : isDocEnd (lvl - 1) rest cont
          list -> goNode lvl list g
          where
            g []    = Right MappingEnd : isDocEnd (lvl - 1) rest cont
            g rest' = goNode lvl rest' g
            mapToList = Map.foldrWithKey (\k v a -> k : v : a) []

        goSeq :: Int -> [Node ()] -> [Node ()] -> Node2EvList -> EvList
        goSeq lvl []  rest cont = Right SequenceEnd : isDocEnd (lvl - 1) rest cont
        goSeq lvl nod rest cont = goNode lvl nod g
          where
            g []    = Right SequenceEnd : isDocEnd (lvl - 1) rest cont
            g rest' = goNode lvl rest' g

        goAnchor :: Int -> NodeId -> Node () -> [Node ()] -> Node2EvList -> EvList
        goAnchor lvl nid nod rest cont = case nod of
          YI.Scalar _ scalar -> goScalar scalar (ancName nid): isDocEnd lvl rest cont
          Mapping   _ tag m  -> Right (MappingStart (ancName nid) (getTag schemaEncoderMapping tag) Block) : goMap (lvl + 1) m rest cont
          Sequence  _ tag s  -> Right (SequenceStart (ancName nid) (getTag schemaEncoderSequence tag) Block) : goSeq (lvl + 1) s rest cont
          Anchor    _ _ _    -> Left "Anchor has a anchor node" : (cont rest)

        isDocEnd :: Int -> [Node ()] -> Node2EvList -> EvList
        isDocEnd lvl rest cont = if lvl == 0 then Right (DocumentEnd (rest /= [])): (cont rest) else (cont rest)

        ancName :: NodeId -> Maybe Anchor
        ancName nid
          | nid == (0-1) = Nothing
          | otherwise    = Just $! T.pack ("a" ++ show nid)

        getTag :: (Tag -> Either String Tag) -> Tag -> Tag
        getTag f tag = case f tag of
          Right t  -> t
          Left err -> error err


