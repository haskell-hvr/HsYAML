{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-3.0
--
module TML (decodeTml) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B

decodeTml :: B.ByteString -> Maybe (ByteString, [(FilePath,ByteString)])
decodeTml s0 = do
    s' <- B.stripPrefix "=== " s0

    let (hdr, rest) = B.break (== '\n') s'
        (_meta, rest') = B.breakSubstring "\n+++ " rest

    ds <- go rest'
    pure (hdr, ds)
  where
    go :: B.ByteString -> Maybe [(FilePath,B.ByteString)]
    go s0
      | B.null s0 = pure []
      | Just s' <- B.stripPrefix "\n+++ " s0 = do
          let (hdr, rest') = B.break (== '\n') s'
              (dat, rest'') = B.breakSubstring "\n+++ " rest'
          dat' <- B.stripPrefix "\n" dat
          xs <- go rest''

          pure ((hdr2fn hdr,unq dat'):xs)

    hdr2fn = map (\c -> if c == '-' then '.' else c) . B.unpack


subst :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
subst old new = B.intercalate new . splitAll old

unq :: B.ByteString -> B.ByteString
unq s0 = subst "<SPC>" " " $ subst "<TAB>" "\t" $ B.unlines ls'
  where
    ls' = map f $ filter (not . B.isPrefixOf "#") $ dropWhileEnd B.null $ ls
    ls = splitAll "\n" s0

    f l | B.isPrefixOf "\\" l = B.drop 1 l
        | otherwise = l

    dropWhileEnd p = reverse . dropWhile p . reverse

splitAll :: B.ByteString -> B.ByteString -> [B.ByteString]
splitAll sep = go
  where
    go s
      | B.null s = []
      | otherwise = case B.breakSubstring sep s of
                      (k,rest) | B.null rest -> [k]
                               | otherwise   -> k : go (B.drop (B.length sep) rest)
