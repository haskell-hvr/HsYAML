{-# LANGUAGE CPP                 #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-3.0
--
module Util
    ( liftEither
    , readMaybe
    , readEither
    , fromIntegerMaybe

    , module X
    ) where

import           Control.Applicative          as X
import           Control.Monad                as X
import           Data.Int                     as X
import           Data.Word                    as X
import           Numeric.Natural              as X (Natural)

#if !MIN_VERSION_mtl(2,2,2) || (__GLASGOW_HASKELL__ == 804 && __GLASGOW_HASKELL_PATCHLEVEL1__ < 2)
import           Control.Monad.Except         (MonadError (throwError))
#else
import           Control.Monad.Except         (liftEither)
#endif

import           Control.Monad.Identity       as X

import           Data.Char                    as X (chr, ord)
import           Data.Map                     as X (Map)
import           Data.Monoid                  as X (Monoid (mappend, mempty))
import           Data.Set                     as X (Set)
import           Data.Text                    as X (Text)

import           Text.ParserCombinators.ReadP as P
import           Text.Read

-- GHC 8.4.1 shipped with a phony `mtl-2.2.2` and so we have to assume
-- pessimistically that on GHC 8.4.1 only, mtl-2.2.2 may be broken (even if it was reinstalled)
#if !MIN_VERSION_mtl(2,2,2) || (__GLASGOW_HASKELL__ == 804 && __GLASGOW_HASKELL_PATCHLEVEL1__ < 2)
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
#endif

#if !MIN_VERSION_base(4,6,0)
readMaybe :: Read a => String -> Maybe a
readMaybe = either (const Nothing) id . readEither

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


fromIntegerMaybe :: forall n . (Integral n, Bounded n) => Integer -> Maybe n
fromIntegerMaybe j
  | l <= j, j <= u  = Just (fromInteger j)
  | otherwise       = Nothing
  where
    u = toInteger (maxBound :: n)
    l = toInteger (minBound :: n)

