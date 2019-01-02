{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- YAML 1.2 Schema resolvers
--
module Data.YAML.Schema
    ( SchemaResolver(..)
    , failsafeSchemaResolver
    , jsonSchemaResolver
    , coreSchemaResolver
    , Scalar(..)

    , tagNull, tagBool, tagStr, tagInt, tagFloat, tagSeq, tagMap
    ) where

import           Control.Monad.Except
import qualified Data.Char            as C
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import           Numeric              (readHex, readOct)
import           Text.Parsec          as P
import           Text.Parsec.Text

import           Data.YAML.Event      (Tag, isUntagged, mkTag, untagged)
import qualified Data.YAML.Event      as YE

import           Util

-- | Primitive scalar types as defined in YAML 1.2
data Scalar = SNull            -- ^ @tag:yaml.org,2002:null@
            | SBool   !Bool    -- ^ @tag:yaml.org,2002:bool@
            | SFloat  !Double  -- ^ @tag:yaml.org,2002:float@
            | SInt    !Integer -- ^ @tag:yaml.org,2002:int@
            | SStr    !Text    -- ^ @tag:yaml.org,2002:str@

            | SUnknown !Tag !Text -- ^ unknown/unsupported tag or untagged (thus unresolved) scalar
            deriving (Eq,Ord,Show)


-- | Definition of a [YAML 1.2 Schema](http://yaml.org/spec/1.2/spec.html#Schema)
--
-- A YAML schema defines how implicit tags are resolved to concrete tags and how data is represented textually in YAML.
data SchemaResolver = SchemaResolver
     { schemaResolverScalar   :: Tag -> YE.ScalarStyle -> T.Text -> Either String Scalar
     , schemaResolverSequence :: Tag -> Either String Tag
     , schemaResolverMapping  :: Tag -> Either String Tag
     , schemaResolverMappingDuplicates :: Bool -- TODO: use something different from 'Bool'
     }


data ScalarTag = ScalarBangTag   -- ^ non-specific ! tag
               | ScalarQMarkTag  -- ^ non-specific ? tag
               | ScalarTag !Tag  -- ^ specific tag

-- common logic for 'schemaResolverScalar'
scalarTag :: (ScalarTag -> T.Text -> Either String Scalar)
             -> Tag -> YE.ScalarStyle -> T.Text -> Either String Scalar
scalarTag f tag sty val = f tag' val
  where
    tag' = case sty of
             YE.Plain
               | tag == untagged -> ScalarQMarkTag -- implicit ? tag

             _ | tag == untagged -> ScalarBangTag -- implicit ! tag
               | tag == tagBang  -> ScalarBangTag -- explicit ! tag
               | otherwise       -> ScalarTag tag


-- | \"Failsafe\" schema resolver as specified
-- in [YAML 1.2 / 10.1.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2803036)
failsafeSchemaResolver :: SchemaResolver
failsafeSchemaResolver = SchemaResolver{..}
  where
    -- scalars
    schemaResolverScalar = scalarTag go
      where
        go ScalarBangTag  v = Right (SStr v)
        go (ScalarTag t)  v
          | t == tagStr     = Right (SStr v)
          | otherwise       = Right (SUnknown t v)
        go ScalarQMarkTag v = Right (SUnknown untagged v) -- leave unresolved

    -- mappings
    schemaResolverMapping t
      | t == tagBang = Right tagMap
      | otherwise    = Right t

    schemaResolverMappingDuplicates = False

    -- sequences
    schemaResolverSequence t
      | t == tagBang = Right tagSeq
      | otherwise    = Right t

-- | Strict JSON schema resolver as specified
-- in [YAML 1.2 / 10.2.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2804356)
jsonSchemaResolver :: SchemaResolver
jsonSchemaResolver = SchemaResolver{..}
  where
    -- scalars
    schemaResolverScalar = scalarTag go
      where
        go ScalarBangTag  v = Right (SStr v)
        go (ScalarTag t)  v
          | t == tagStr     = Right (SStr v)
          | t == tagNull  = if isNullLiteral v then Right SNull else Left ("invalid !!null " ++ show v)
          | t == tagInt   = maybe (Left $ "invalid !!int " ++ show v)   (Right . SInt)   $ jsonDecodeInt   v
          | t == tagFloat = maybe (Left $ "invalid !!float " ++ show v) (Right . SFloat) $ jsonDecodeFloat v
          | t == tagBool  = maybe (Left $ "invalid !!bool " ++ show v)  (Right . SBool)  $ jsonDecodeBool  v
          | otherwise       = Right (SUnknown t v) -- unknown specific tag
        go ScalarQMarkTag v
          | isNullLiteral v             = Right SNull
          | Just b <- jsonDecodeBool  v = Right $! SBool b
          | Just i <- jsonDecodeInt   v = Right $! SInt i
          | Just f <- jsonDecodeFloat v = Right $! SFloat f
          | otherwise = Right (SUnknown untagged v) -- leave unresolved  -- FIXME: YAML 1.2 spec requires an error here

    isNullLiteral = (== "null")

    -- mappings
    schemaResolverMapping t
      | t == tagBang = Right tagMap
      | isUntagged t = Right tagMap
      | otherwise    = Right t

    schemaResolverMappingDuplicates = False

    -- sequences
    schemaResolverSequence t
      | t == tagBang = Right tagSeq
      | isUntagged t = Right tagSeq
      | otherwise    = Right t

-- | Core schema resolver as specified
-- in [YAML 1.2 / 10.3.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2805071)
coreSchemaResolver :: SchemaResolver
coreSchemaResolver = SchemaResolver{..}
  where
    -- scalars
    schemaResolverScalar = scalarTag go
      where
        go ScalarBangTag  v = Right (SStr v)
        go (ScalarTag t)  v
          | t == tagStr     = Right (SStr v)
          | t == tagNull  = if isNullLiteral v then Right SNull else Left ("invalid !!null " ++ show v)
          | t == tagInt   = maybe (Left $ "invalid !!int " ++ show v)   (Right . SInt)   $ coreDecodeInt   v
          | t == tagFloat = maybe (Left $ "invalid !!float " ++ show v) (Right . SFloat) $ coreDecodeFloat v
          | t == tagBool  = maybe (Left $ "invalid !!bool " ++ show v)  (Right . SBool)  $ coreDecodeBool  v
          | otherwise       = Right (SUnknown t v) -- unknown specific tag
        go ScalarQMarkTag v
          | isNullLiteral v             = Right SNull
          | Just b <- coreDecodeBool  v = Right $! SBool b
          | Just i <- coreDecodeInt   v = Right $! SInt i
          | Just f <- coreDecodeFloat v = Right $! SFloat f
          | otherwise = Right (SStr v) -- map to !!str by default

    isNullLiteral = flip Set.member (Set.fromList [ "", "null", "NULL", "Null", "~" ])

    -- mappings
    schemaResolverMapping t
      | t == tagBang = Right tagMap
      | isUntagged t = Right tagMap
      | otherwise    = Right t

    schemaResolverMappingDuplicates = False

    -- sequences
    schemaResolverSequence t
      | t == tagBang = Right tagSeq
      | isUntagged t = Right tagSeq
      | otherwise    = Right t


-- | @tag:yaml.org,2002:bool@ (JSON Schema)
jsonDecodeBool :: T.Text -> Maybe Bool
jsonDecodeBool "false" = Just False
jsonDecodeBool "true"  = Just True
jsonDecodeBool _       = Nothing

-- | @tag:yaml.org,2002:bool@ (Core Schema)
coreDecodeBool :: T.Text -> Maybe Bool
coreDecodeBool = flip Map.lookup $
  Map.fromList [ ("true", True)
               , ("True", True)
               , ("TRUE", True)
               , ("false", False)
               , ("False", False)
               , ("FALSE", False)
               ]

-- | @tag:yaml.org,2002:int@ according to JSON Schema
--
-- > 0 | -? [1-9] [0-9]*
jsonDecodeInt  :: T.Text -> Maybe Integer
jsonDecodeInt t | T.null t = Nothing
jsonDecodeInt "0" = Just 0
jsonDecodeInt t = do
  -- [-]? [1-9] [0-9]*
  let tabs | T.isPrefixOf "-" t = T.tail t
           | otherwise          = t

  guard (not (T.null tabs))
  guard (T.head tabs /= '0')
  guard (T.all C.isDigit tabs)

  readMaybe (T.unpack t)

-- | @tag:yaml.org,2002:int@ according to Core Schema
--
-- > [-+]? [0-9]+         (Base 10)
-- > 0o [0-7]+            (Base 8)
-- > 0x [0-9a-fA-F]+      (Base 16)
--
coreDecodeInt :: T.Text -> Maybe Integer
coreDecodeInt t
  | T.null t = Nothing

  -- > 0x [0-9a-fA-F]+      (Base 16)
  | Just rest <- T.stripPrefix "0x" t
  , T.all C.isHexDigit rest
  , [(j,"")] <- readHex (T.unpack rest)
  = Just $! j

  -- 0o [0-7]+            (Base 8)
  | Just rest <- T.stripPrefix "0o" t
  , T.all C.isOctDigit rest
  , [(j,"")] <- readOct (T.unpack rest)
  = Just $! j

  -- [-+]? [0-9]+         (Base 10)
  | T.all C.isDigit t
  = Just $! read (T.unpack t)

  | Just rest <- T.stripPrefix "+" t
  , not (T.null rest)
  , T.all C.isDigit rest
  = Just $! read (T.unpack rest)

  | Just rest <- T.stripPrefix "-" t
  , not (T.null rest)
  , T.all C.isDigit rest
  = Just $! read (T.unpack t)

  | otherwise = Nothing


-- | @tag:yaml.org,2002:float@ according to JSON Schema
--
-- > -? ( 0 | [1-9] [0-9]* ) ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )?
--
jsonDecodeFloat :: T.Text -> Maybe Double
jsonDecodeFloat = either (const Nothing) Just . parse float ""
  where
    float :: Parser Double
    float = do
      -- -?
      p0 <- option "" ("-" <$ char '-')

      -- ( 0 | [1-9] [0-9]* )
      p1 <- do
        d <- digit
        if (d /= '0')
          then (d:) <$> P.many digit
          else pure [d]

      -- ( \. [0-9]* )?
      p2 <- option "" $ (:) <$> char '.' <*> option "0" (many1 digit)

      -- ( [eE] [-+]? [0-9]+ )?
      p3 <- option "" $ do
        void (char 'e' P.<|> char 'E')
        s <- option "" (("-" <$ char '-') P.<|> ("" <$ char '+'))
        d <- P.many1 digit

        pure ("e" ++ s ++ d)

      eof

      let t' = p0++p1++p2++p3
      pure $! read t'

-- | @tag:yaml.org,2002:float@ according to Core Schema
--
-- > [-+]? ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? ) ( [eE] [-+]? [0-9]+ )?
--
coreDecodeFloat :: T.Text -> Maybe Double
coreDecodeFloat t
  | Just j <- Map.lookup t literals = Just j -- short-cut
  | otherwise = either (const Nothing) Just . parse float "" $ t
  where
    float :: Parser Double
    float = do
      -- [-+]?
      p0 <- option "" (("-" <$ char '-') P.<|> "" <$ char '+')

      -- ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? )
      p1 <- (char '.' *> (("0."++) <$> many1 digit))
            P.<|> do d1  <- many1 digit
                     d2  <- option "" $ (:) <$> char '.' <*> option "0" (many1 digit)
                     pure (d1++d2)

      -- ( [eE] [-+]? [0-9]+ )?
      p2 <- option "" $ do
        void (char 'e' P.<|> char 'E')
        s <- option "" (("-" <$ char '-') P.<|> ("" <$ char '+'))
        d <- P.many1 digit

        pure ("e" ++ s ++ d)

      eof

      let t' = p0++p1++p2

      pure $! read t'

    literals = Map.fromList
      [ ("0"   , 0)

      , (".nan", (0/0))
      , (".NaN", (0/0))
      , (".NAN", (0/0))

      , (".inf", (1/0))
      , (".Inf", (1/0))
      , (".INF", (1/0))

      , ("+.inf", (1/0))
      , ("+.Inf", (1/0))
      , ("+.INF", (1/0))

      , ("-.inf", (-1/0))
      , ("-.Inf", (-1/0))
      , ("-.INF", (-1/0))
      ]


tagNull, tagBool, tagStr, tagInt, tagFloat, tagSeq, tagMap, tagBang :: Tag
tagNull  = mkTag "tag:yaml.org,2002:null"
tagStr   = mkTag "tag:yaml.org,2002:str"
tagInt   = mkTag "tag:yaml.org,2002:int"
tagFloat = mkTag "tag:yaml.org,2002:float"
tagBool  = mkTag "tag:yaml.org,2002:bool"
tagSeq   = mkTag "tag:yaml.org,2002:seq"
tagMap   = mkTag "tag:yaml.org,2002:map"
tagBang  = mkTag "!"
