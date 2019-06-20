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
type Node2EvStream = [Node ()] -> [Either String Event]


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

dumpEvents :: Node2EvStream
dumpEvents nodes = Right StreamStart: go0 nodes
  where
    go0 :: [Node ()] -> EvList
    go0 [] = [Right StreamEnd]
    go0 n = Right (DocumentStart NoDirEndMarker): goNode 0 n (\ev -> go0 ev)


    goNode :: Int -> [Node ()] -> Node2EvStream -> EvList
    goNode _ [] _ = [Left "Dumper: unexpected pattern in goNode"]
    goNode lvl (node: rest) cont = case node of 
        YI.Scalar _ scalar -> goScalar scalar : isDocEnd lvl rest cont
        Mapping   _ tag m  -> Right (MappingStart Nothing tag Block) : goMap (lvl+1) m rest cont
        Sequence  _ tag s  -> Right (SequenceStart Nothing tag Block) : goSeq (lvl+1) s rest cont
        Anchor    _ _ _    -> [Left "TODO"]

    goScalar :: YS.Scalar -> Either String Event
    goScalar scalar = case scalar of
        SNull         -> Right (YE.Scalar Nothing (Tag Nothing) Plain (T.pack ""))    
        SBool  bool   -> Right (YE.Scalar Nothing (Tag Nothing) Plain (T.pack . show $ bool))
        SFloat double -> Right (YE.Scalar Nothing (Tag Nothing) Plain (T.pack . show $ double))
        SInt   int    -> Right (YE.Scalar Nothing (Tag Nothing) Plain (T.pack . show $ int))
        SStr   text   -> Right (YE.Scalar Nothing (Tag Nothing) Plain text)
        SUnknown _ _  -> Left "SUnknown Scalar type"

    goMap :: Int -> Map.Map (Node ()) (Node ()) -> [Node ()] -> Node2EvStream -> EvList
    goMap lvl m rest cont =  goNode lvl (tupleToList (Map.toList m)) g
        where 
          g [] = (Right MappingEnd) : isDocEnd (lvl - 1) rest cont
          g rest' = goNode lvl rest' g 
          tupleToList = foldr (\(f,s) a -> f : s : a) []

    goSeq :: Int -> [Node ()] -> [Node ()] -> Node2EvStream -> EvList
    goSeq lvl nod rest cont =  goNode lvl nod g
        where 
          g [] = (Right SequenceEnd) : isDocEnd (lvl - 1) rest cont
          g rest' = goNode lvl rest' g 

    isDocEnd :: Int -> [Node ()] -> Node2EvStream -> EvList
    isDocEnd lvl rest cont = if lvl == 0 then Right (DocumentEnd (rest /= [])): (cont rest) else (cont rest)
