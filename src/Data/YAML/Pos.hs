{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Pos
    ( Pos(..)
    , prettyPosWithSource
    ) where

import qualified Data.ByteString.Lazy     as BL
import qualified Data.YAML.Token.Encoding as Enc
import           Util

-- | Position in parsed YAML source
--
-- See also 'prettyPosWithSource'.
--
-- __NOTE__: if 'posCharOffset' is negative the 'Pos' value doesn't refer to a proper location; this may be emitted in corner cases when no proper location can be inferred.
data Pos = Pos
    { posByteOffset :: !Int -- ^ 0-based byte offset
    , posCharOffset :: !Int -- ^ 0-based character (Unicode code-point) offset
    , posLine       :: !Int -- ^ 1-based line number
    , posColumn     :: !Int -- ^ 0-based character (Unicode code-point) column number
    } deriving (Eq, Show, Generic)

-- | @since 0.2.0
instance NFData Pos where rnf !_ = ()

-- | Pretty prints a 'Pos' together with the line the 'Pos' refers and the column position.
--
-- The input 'BL.ByteString' must be the same that was passed to the
-- YAML decoding function that produced the 'Pos' value. The 'String'
-- argument is inserted right after the @<line>:<column>:@ in the
-- first line. The pretty-printed position result 'String' will be
-- terminated by a trailing newline.
--
-- For instance,
--
-- @
-- 'prettyPosWithSource' somePos someInput " error" ++ "unexpected character\\n"
-- @ results in
--
-- > 11:7: error
-- >     |
-- >  11 | foo: | bar
-- >     |        ^
-- > unexpected character
--
-- @since 0.2.1
prettyPosWithSource :: Pos -> BL.ByteString -> String -> String
prettyPosWithSource Pos{..} source msg
  | posCharOffset < 0 || posByteOffset < 0 = "0:0:" ++ msg ++ "\n" -- unproper location
  | otherwise = unlines
    [ show posLine ++ ":" ++ show posColumn ++ ":" ++ msg
    , lpfx
    , lnostr ++ "| " ++ line
    , lpfx ++ replicate posColumn ' ' ++ "^"
    ]

  where
    lnostr = " " ++ show posLine ++ " "
    lpfx   = (' ' <$ lnostr) ++ "| "

    (_,lstart) = findLineStartByByteOffset posByteOffset source
    line = map snd $ takeWhile (not . isNL . snd) lstart

    isNL c = c == '\r' || c == '\n'

findLineStartByByteOffset :: Int -> BL.ByteString -> (Int,[(Int,Char)])
findLineStartByByteOffset bofs0 input = go 0 inputChars inputChars
  where
    (_,inputChars) = Enc.decode input

    go lsOfs lsChars [] = (lsOfs,lsChars)
    go lsOfs lsChars ((ofs',_):_)
      | bofs0 < ofs' = (lsOfs,lsChars)

    go _ _ ((_,'\r'):(ofs','\n'):rest) = go ofs' rest rest
    go _ _ ((ofs','\r'):rest) = go ofs' rest rest
    go _ _ ((ofs','\n'):rest) = go ofs' rest rest
    go lsOfs lsChars (_:rest) = go lsOfs lsChars rest
