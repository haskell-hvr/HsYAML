{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Pos where

import           Util

-- | Position in parsed YAML source
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
