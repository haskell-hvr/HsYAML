{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Util
    ( liftEither'
    , readMaybe
    , readEither
    , fromIntegerMaybe
    , (<>)

    , mapFromListNoDupes
    , mapInsertNoDupe

    , bsToStrict

    , module X
    ) where

import           Control.Applicative          as X
import           Control.DeepSeq              as X (NFData (rnf))
import           Control.Monad                as X
import           Data.Functor                 as X
import           Data.Int                     as X
import           Data.Word                    as X
import           GHC.Generics                 as X (Generic)
import           Numeric.Natural              as X (Natural)

import           Control.Monad.Except         as X (ExceptT (..), MonadError (..), runExceptT)
import           Control.Monad.Identity       as X

import           Data.Char                    as X (chr, ord)
import           Data.Map                     as X (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  as X (Monoid (mappend, mempty))
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup               ((<>))
#else
import           Data.Monoid                  ((<>))
#endif
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BS.L
import           Data.Set                     as X (Set)
import           Data.Text                    as X (Text)

import           Text.ParserCombinators.ReadP as P
import           Text.Read

-- GHC 8.4.1 shipped with a phony `mtl-2.2.2` and so we have no
-- bulletproof way to know when `Control.Monad.Except` exports liftEither
-- or not; after NixOS managed to break an otherwise effective workaround
-- I'll just throwing my hands up in the air and will consider
-- `Control.Monad.Except.liftEither` scorched earth for now.
liftEither' :: MonadError e m => Either e a -> m a
liftEither' = either throwError return


#if !MIN_VERSION_base(4,6,0)

-- | Parse a string using the 'Read' instance. Succeeds if there is
-- exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe = either (const Nothing) id . readEither

-- | Parse a string using the 'Read' instance. Succeeds if there is
-- exactly one valid result. A 'Left' value indicates a parse error.
readEither :: Read a => String -> Either String a
readEither s = case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
                 [x] -> Right x
                 []  -> Left "Prelude.read: no parse"
                 _   -> Left "Prelude.read: ambiguous parse"
 where
  read' = do x <- readPrec
             Text.Read.lift P.skipSpaces
             return x
#endif

-- | Succeeds if the 'Integral' value is in the bounds of the given Data type.
-- 'Nothing' indicates that the value is outside the bounds.
fromIntegerMaybe :: forall n . (Integral n, Bounded n) => Integer -> Maybe n
fromIntegerMaybe j
  | l <= j, j <= u  = Just (fromInteger j)
  | otherwise       = Nothing
  where
    u = toInteger (maxBound :: n)
    l = toInteger (minBound :: n)


-- | A convience wrapper over 'mapInsertNoDupe'
mapFromListNoDupes :: Ord k => [(k,a)] -> Either (k,a) (Map k a)
mapFromListNoDupes = go mempty
  where
    go !m [] = Right m
    go !m ((k,!v):rest) = case mapInsertNoDupe k v m of
                            Nothing -> Left (k,v)
                            Just m' -> go m' rest

-- | A convience wrapper over 'Data.Map.insertLookupWithKey'
mapInsertNoDupe :: Ord k => k -> a -> Map k a -> Maybe (Map k a)
mapInsertNoDupe kx x t = case Map.insertLookupWithKey (\_ a _ -> a) kx x t of
                           (Nothing, m) -> Just m
                           (Just _, _)  -> Nothing


-- | Equivalent to the function 'Data.ByteString.toStrict'.
-- O(n) Convert a lazy 'BS.L.ByteString' into a strict 'BS.ByteString'.
{-# INLINE bsToStrict #-}
bsToStrict :: BS.L.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,10,0)
bsToStrict = BS.L.toStrict
#else
bsToStrict = BS.concat . BS.L.toChunks
#endif
