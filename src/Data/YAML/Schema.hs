{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Predefined YAML 1.2 Schema resolvers and encoders as well as support for defining custom resolvers and encoders.
--
-- @since 0.2.0.0
module Data.YAML.Schema
    ( -- * Schema resolvers
      -- ** YAML 1.2 Schema resolvers
      SchemaResolver(..)
    , failsafeSchemaResolver
    , jsonSchemaResolver
    , coreSchemaResolver

      -- * Schema encoders
      -- ** YAML 1.2 Schema encoders
    , SchemaEncoder(..)
    , failsafeSchemaEncoder
    , jsonSchemaEncoder
    , coreSchemaEncoder

      -- ** Custom Schema encoding
      --
      -- | According to YAML 1.2 the recommended default 'SchemaEncoder' is 'coreSchemaEncoder' under which 'Scalar's are encoded as follows:
      --
      -- * String which are made of Plain Characters (see 'isPlainChar'), unambiguous (see 'isAmbiguous') and do not contain any leading/trailing spaces are encoded as 'Data.YAML.Event.Plain' 'Scalar'.
      --
      -- * Rest of the strings are encoded in DoubleQuotes
      --
      -- * Booleans are encoded using 'encodeBool'
      --
      -- * Double values are encoded using 'encodeDouble'
      --
      -- * Integral values are encoded using 'encodeInt'
      --
    , setScalarStyle
    , isPlainChar
    , isAmbiguous
    , encodeDouble
    , encodeBool
    , encodeInt
    ) where

import           Data.YAML.Schema.Internal
