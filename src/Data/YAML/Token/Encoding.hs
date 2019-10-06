{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

-- |
-- Copyright: © Oren Ben-Kiki 2007,
--            © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- UTF decoding
--
-- This really should be factored out to the standard libraries. Since it isn't
-- there, we get to tailor it exactly to our needs. We use lazy byte strings as
-- input, which should give reasonable I\/O performance when reading large
-- files. The output is a normal 'Char' list which is easy to work with and
-- should be efficient enough as long as the 'Data.YAML.Token.Parser' does its job right.
--
module Data.YAML.Token.Encoding
  ( decode
  , Encoding(..)
  ) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Util

-- | Denotes the /Unicode Transformation Format/ (UTF) used for serializing the YAML document
data Encoding = UTF8    -- ^ UTF-8 encoding (or ASCII)
              | UTF16LE -- ^ UTF-16 little endian
              | UTF16BE -- ^ UTF-16 big endian
              | UTF32LE -- ^ UTF-32 little endian
              | UTF32BE -- ^ UTF-32 big endian
              deriving (Eq,Generic)

-- | @show encoding@ converts an 'Encoding' to the encoding name (with a "-")
-- as used by most programs.
instance Show Encoding where
    show UTF8    = "UTF-8"
    show UTF16LE = "UTF-16LE"
    show UTF16BE = "UTF-16BE"
    show UTF32LE = "UTF-32LE"
    show UTF32BE = "UTF-32BE"

-- | @since 0.2.0
instance NFData Encoding where rnf !_ = ()

-- | @'decode' bytes@ automatically detects the 'Encoding' used and converts the
-- /bytes/ to Unicode characters, with byte offsets. Note the offset is for
-- past end of the character, not its beginning.
decode :: BLC.ByteString -> (Encoding, [(Int, Char)])
decode text = (encoding, undoEncoding encoding text)
  where
    encoding = detectEncoding $ BL.unpack $ BL.take 4 text

-- | @'detectEncoding' text@ examines the first few chars (bytes) of the /text/
-- to deduce the Unicode encoding used according to the YAML spec.
detectEncoding :: [Word8] -> Encoding
detectEncoding text = case text of
    0x00 : 0x00 : 0xFE : 0xFF : _ -> UTF32BE
    0x00 : 0x00 : 0x00 : _    : _ -> UTF32BE
    0xFF : 0xFE : 0x00 : 0x00 : _ -> UTF32LE
    _    : 0x00 : 0x00 : 0x00 : _ -> UTF32LE
    0xFE : 0xFF : _               -> UTF16BE
    0x00 : _    : _               -> UTF16BE
    0xFF : 0xFE : _               -> UTF16LE
    _    : 0x00 : _               -> UTF16LE
    0xEF : 0xBB : 0xBF : _        -> UTF8
    _                             -> UTF8

-- | @undoEncoding encoding bytes@ converts a /bytes/ stream to Unicode
-- characters according to the /encoding/.
undoEncoding :: Encoding -> BLC.ByteString -> [(Int, Char)]
undoEncoding encoding bytes =
  case encoding of
    UTF8    -> undoUTF8 bytes 0
    UTF16LE -> combinePairs $ undoUTF16LE bytes 0
    UTF16BE -> combinePairs $ undoUTF16BE bytes 0
    UTF32LE -> validateScalars $ undoUTF32LE bytes 0
    UTF32BE -> validateScalars $ undoUTF32BE bytes 0
  where
    validateScalars [] = []
    validateScalars (x@(_,c):rest)
      | '\xD800' <= c, c <= '\xDFFF' = error "UTF-32 stream contains invalid surrogate code-point"
      | otherwise                    = x : validateScalars rest

-- ** UTF-32 decoding

-- | @hasFewerThan bytes n@ checks whether there are fewer than /n/ /bytes/
-- left to read.
hasFewerThan :: Int -> BLC.ByteString -> Bool
hasFewerThan n bytes
  | n == 1 = BLC.null bytes
  | n  > 1 = BLC.null bytes || hasFewerThan (n - 1) (BLC.tail bytes)
  | otherwise = False

-- | @undoUTF32LE bytes offset@ decoded a UTF-32LE /bytes/ stream to Unicode
-- chars.
undoUTF32LE :: BLC.ByteString -> Int -> [(Int, Char)]
undoUTF32LE bytes offset
  | BLC.null bytes = []
  | hasFewerThan 4 bytes = error "UTF-32LE input contains invalid number of bytes"
  | otherwise = let first    = BLC.head bytes
                    bytes'   = BLC.tail bytes
                    second   = BLC.head bytes'
                    bytes''  = BLC.tail bytes'
                    third    = BLC.head bytes''
                    bytes''' = BLC.tail bytes''
                    fourth   = BLC.head bytes'''
                    rest     = BLC.tail bytes'''
                in (offset + 4,
                    chr $        ord first
                        + 256 * (ord second
                        + 256 * (ord third
                        + 256 *  ord fourth))):(undoUTF32LE rest $ offset + 4)

-- | @undoUTF32BE bytes offset@ decoded a UTF-32BE /bytes/ stream to Unicode
-- chars.
undoUTF32BE :: BLC.ByteString -> Int -> [(Int, Char)]
undoUTF32BE bytes offset
  | BLC.null bytes = []
  | hasFewerThan 4 bytes = error "UTF-32BE input contains invalid number of bytes"
  | otherwise = let first    = BLC.head bytes
                    bytes'   = BLC.tail bytes
                    second   = BLC.head bytes'
                    bytes''  = BLC.tail bytes'
                    third    = BLC.head bytes''
                    bytes''' = BLC.tail bytes''
                    fourth   = BLC.head bytes'''
                    rest     = BLC.tail bytes'''
                in (offset + 4,
                    chr $        ord fourth
                        + 256 * (ord third
                        + 256 * (ord second
                        + 256 *  ord first))):(undoUTF32BE rest $ offset + 4)

-- ** UTF-16 decoding

-- | @combinePairs chars@ converts each pair of UTF-16 surrogate characters to a
-- single Unicode character.
combinePairs :: [(Int, Char)] -> [(Int, Char)]
combinePairs []                          = []
combinePairs (head'@(_, head_char):tail')
  | '\xD800' <= head_char && head_char <= '\xDBFF' = combineLead head' tail'
  | '\xDC00' <= head_char && head_char <= '\xDFFF' = error "UTF-16 contains trail surrogate without lead surrogate"
  | otherwise                                      = head':combinePairs tail'

-- | @combineLead lead rest@ combines the /lead/ surrogate with the head of the
-- /rest/ of the input chars, assumed to be a /trail/ surrogate, and continues
-- combining surrogate pairs.
combineLead :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
combineLead _lead []                                 = error "UTF-16 contains lead surrogate as final character"
combineLead (_, lead_char) ((trail_offset, trail_char):rest)
  | '\xDC00' <= trail_char && trail_char <= '\xDFFF' = (trail_offset, combineSurrogates lead_char trail_char):combinePairs rest
  | otherwise                                        = error "UTF-16 contains lead surrogate without trail surrogate"

-- | @surrogateOffset@ is copied from the Unicode FAQs.
surrogateOffset :: Int
surrogateOffset = 0x10000 - (0xD800 * 1024) - 0xDC00

-- | @combineSurrogates lead trail@ combines two UTF-16 surrogates into a single
-- Unicode character.
combineSurrogates :: Char -> Char -> Char
combineSurrogates lead trail = chr $ ord lead * 1024 + ord trail + surrogateOffset

-- | @undoUTF18LE bytes offset@ decoded a UTF-16LE /bytes/ stream to Unicode
-- chars.
undoUTF16LE :: BLC.ByteString -> Int -> [(Int, Char)]
undoUTF16LE bytes offset
  | BLC.null bytes = []
  | hasFewerThan 2 bytes = error "UTF-16LE input contains odd number of bytes"
  | otherwise = let low    = BLC.head bytes
                    bytes' = BLC.tail bytes
                    high   = BLC.head bytes'
                    rest   = BLC.tail bytes'
                in (offset + 2, chr $ ord low + ord high * 256):(undoUTF16LE rest $ offset + 2)

-- | @undoUTF18BE bytes offset@ decoded a UTF-16BE /bytes/ stream to Unicode
-- chars.
undoUTF16BE :: BLC.ByteString -> Int -> [(Int, Char)]
undoUTF16BE bytes offset
  | BLC.null bytes = []
  | hasFewerThan 2 bytes = error "UTF-16BE input contains odd number of bytes"
  | otherwise = let high   = BLC.head bytes
                    bytes' = BLC.tail bytes
                    low    = BLC.head bytes'
                    rest   = BLC.tail bytes'
                in (offset + 2, chr $ ord low + ord high * 256):(undoUTF16BE rest $ offset + 2)

-- ** UTF-8 decoding

-- | @undoUTF8 bytes offset@ decoded a UTF-8 /bytes/ stream to Unicode chars.
undoUTF8 :: BLC.ByteString -> Int -> [(Int, Char)]
undoUTF8 bytes = undoUTF8' (BL.unpack bytes)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

w2i :: Word8 -> Int
w2i = fromIntegral

undoUTF8' :: [Word8] -> Int -> [(Int, Char)]
undoUTF8' [] _ = []
undoUTF8' (first:rest) !offset
  | first < 0x80  = (offset', c) : undoUTF8' rest offset'
  where
    !offset' = offset + 1
    !c       = w2c first
undoUTF8' (first:rest) !offset
  | first < 0xC0  = error "UTF-8 input contains invalid first byte"
  | first < 0xE0  = decodeTwoUTF8   first offset rest
  | first < 0xF0  = decodeThreeUTF8 first offset rest
  | first < 0xF8  = decodeFourUTF8  first offset rest
  | otherwise     = error "UTF-8 input contains invalid first byte"

-- | @decodeTwoUTF8 first offset bytes@ decodes a two-byte UTF-8 character,
-- where the /first/ byte is already available and the second is the head of
-- the /bytes/, and then continues to undo the UTF-8 encoding.
decodeTwoUTF8 :: Word8 -> Int -> [Word8] -> [(Int, Char)]
decodeTwoUTF8 first offset (second:rest)
  | second < 0x80 || 0xBF < second = error "UTF-8 double byte char has invalid second byte"
  | otherwise = (offset', c) : undoUTF8' rest offset'
  where
    !offset' = offset + 2
    !c       = chr ((w2i first - 0xc0) * 0x40  + (w2i second - 0x80))
decodeTwoUTF8 _ _ [] = error "UTF-8 double byte char is missing second byte at eof"

-- | @decodeThreeUTF8 first offset bytes@ decodes a three-byte UTF-8 character,
-- where the /first/ byte is already available and the second and third are the
-- head of the /bytes/, and then continues to undo the UTF-8 encoding.
decodeThreeUTF8 :: Word8 -> Int -> [Word8] -> [(Int, Char)]
decodeThreeUTF8 first offset (second:third:rest)
  | second < 0x80 || 0xBF < second = error "UTF-8 triple byte char has invalid second byte"
  | third <  0x80 || 0xBF < third  = error "UTF-8 triple byte char has invalid third byte"
  | otherwise = (offset', c): undoUTF8' rest offset'
  where
    !offset' = offset + 3
    !c       = chr((w2i first  - 0xE0) * 0x1000 +
                   (w2i second - 0x80) * 0x40 +
                   (w2i third  - 0x80))
decodeThreeUTF8 _ _ _ =error "UTF-8 triple byte char is missing bytes at eof"

-- | @decodeFourUTF8 first offset bytes@ decodes a four-byte UTF-8 character,
-- where the /first/ byte is already available and the second, third and fourth
-- are the head of the /bytes/, and then continues to undo the UTF-8 encoding.
decodeFourUTF8 :: Word8 -> Int -> [Word8] -> [(Int, Char)]
decodeFourUTF8 first offset (second:third:fourth:rest)
  | second < 0x80 || 0xBF < second = error "UTF-8 quad byte char has invalid second byte"
  | third  < 0x80 || 0xBF < third  = error "UTF-8 quad byte char has invalid third byte"
  | third  < 0x80 || 0xBF < third  = error "UTF-8 quad byte char has invalid fourth byte"
  | otherwise                      = (offset', c) : undoUTF8' rest offset'
  where
    !offset' = offset + 4
    !c       = chr((w2i first  - 0xF0) * 0x40000 +
                   (w2i second - 0x80) * 0x1000 +
                   (w2i third  - 0x80) * 0x40 +
                   (w2i fourth - 0x80))

decodeFourUTF8 _ _ _ = error "UTF-8 quad byte char is missing bytes at eof"
