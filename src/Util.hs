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

import           Control.Monad.Except
import           Control.Monad.Identity       as X

import           Data.Map                     as X (Map)
import           Data.Monoid                  as X (Monoid (mappend, mempty))
import           Data.Set                     as X (Set)
import           Data.Text                    as X (Text)

import           Text.ParserCombinators.ReadP as P
import           Text.Read

#if !MIN_VERSION_mtl(2,2,2)
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

