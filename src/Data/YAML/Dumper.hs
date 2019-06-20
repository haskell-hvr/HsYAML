{-# LANGUAGE Safe              #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.YAML.Dumper
    ( dumpYAML
    , dumpYAML'
    , dumpEvents
    ) where

import           Data.YAML.Event.Internal as YE
import           Data.YAML.Internal       as YI
import           Data.YAML.Schema         as YS
import           Data.YAML.Event.Writer   (writeEvents)

import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.Map                 as Map
import qualified Data.Text                as T


type EvList = [Either String Event]
type Node2EvList = [Node ()] -> EvList

-- | Dump YAML Nodes using specified UTF encoding to a lazy 'BS.L.ByteString'
--
-- @since 0.2.0
dumpYAML ::  Encoding -> [Node ()] -> BS.L.ByteString
dumpYAML encoding nodes = writeEvents encoding $ case sequence (dumpEvents nodes) of
    Left str -> error str
    Right ev -> ev

-- | Convenience wrapper over 'dumpYAML' expecting exactly one YAML document
--
-- @since 0.2.0
dumpYAML' ::  Encoding -> Node () -> BS.L.ByteString
dumpYAML' encoding node = dumpYAML encoding [node] 

dumpEvents :: Node2EvList
dumpEvents nodes = Right StreamStart: go0 nodes
  where
    go0 :: [Node ()] -> EvList
    go0 [] = [Right StreamEnd]
    go0 n = Right (DocumentStart NoDirEndMarker): goNode 0 n (\ev -> go0 ev)


    goNode :: Int -> [Node ()] -> Node2EvList -> EvList
    goNode _ [] _ = [Left "Dumper: unexpected pattern in goNode"]
    goNode lvl (node: rest) cont = case node of 
        YI.Scalar _ scalar -> goScalar scalar Nothing: isDocEnd lvl rest cont
        Mapping   _ tag m  -> Right (MappingStart Nothing tag Block) : goMap (lvl+1) m rest cont
        Sequence  _ tag s  -> Right (SequenceStart Nothing tag Block) : goSeq (lvl+1) s rest cont
        Anchor    _ nid n  -> goAnchor lvl nid n rest cont

    goScalar :: YS.Scalar -> Maybe Anchor -> Either String Event
    goScalar scalar anc = case scalar of
        SNull         -> Right (YE.Scalar anc (Tag Nothing) Plain (T.pack ""))    
        SBool  bool   -> Right (YE.Scalar anc (Tag Nothing) Plain (T.pack . show $ bool))
        SFloat double -> Right (YE.Scalar anc (Tag Nothing) Plain (T.pack . show $ double))
        SInt   int    -> Right (YE.Scalar anc (Tag Nothing) Plain (T.pack . show $ int))
        SStr   text   -> Right (YE.Scalar anc (Tag Nothing) Plain text)
        SUnknown _ _  -> Left "SUnknown Scalar type"

    goMap :: Int -> Map.Map (Node ()) (Node ()) -> [Node ()] -> Node2EvList -> EvList
    goMap lvl m rest cont =  goNode lvl (mapToList m) g
        where 
          g [] = (Right MappingEnd) : isDocEnd (lvl - 1) rest cont
          g rest' = goNode lvl rest' g 
          mapToList = Map.foldrWithKey (\k v a -> k : v : a) []

    goSeq :: Int -> [Node ()] -> [Node ()] -> Node2EvList -> EvList
    goSeq lvl nod rest cont =  goNode lvl nod g
        where 
          g [] = (Right SequenceEnd) : isDocEnd (lvl - 1) rest cont
          g rest' = goNode lvl rest' g 

    goAnchor :: Int -> NodeId -> Node () -> [Node ()] -> Node2EvList -> EvList
    goAnchor lvl nid nod rest cont = case nod of 
        YI.Scalar _ scalar -> goScalar scalar (ancName nid): isDocEnd lvl rest cont
        Mapping   _ tag m  -> Right (MappingStart (ancName nid) tag Block) : goMap (lvl+1) m rest cont
        Sequence  _ tag s  -> Right (SequenceStart (ancName nid) tag Block) : goSeq (lvl+1) s rest cont
        Anchor    _ _ _    -> Left "Anchor has a anchor node" : (cont rest)

    isDocEnd :: Int -> [Node ()] -> Node2EvList -> EvList
    isDocEnd lvl rest cont = if lvl == 0 then Right (DocumentEnd (rest /= [])): (cont rest) else (cont rest)

    ancName :: NodeId -> Maybe Anchor
    ancName (-1) = Nothing
    ancName nid  = Just $ T.pack ("a" ++ show nid)
