{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- YAML 1.2 Schema resolvers and encoders
--
module Data.YAML.Schema.Internal
    ( SchemaResolver(..)
    , failsafeSchemaResolver
    , jsonSchemaResolver
    , coreSchemaResolver
    , Scalar(..)

    , SchemaEncoder(..)
    , failsafeSchemaEncoder
    , jsonSchemaEncoder
    , coreSchemaEncoder

    , tagNull, tagBool, tagStr, tagInt, tagFloat, tagSeq, tagMap

    , isPlainChar , isAmbiguous, defaultSchemaEncoder, setScalarStyle
    , encodeDouble, encodeBool, encodeInt
    ) where

import qualified Data.Char        as C
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text        as T
import           Numeric          (readHex, readOct)
import           Text.Parsec      as P
import           Text.Parsec.Text

import           Data.YAML.Event  (ScalarStyle (..), Tag, isUntagged, mkTag, untagged)
import qualified Data.YAML.Event  as YE

import           Util

-- | Primitive scalar types as defined in YAML 1.2
data Scalar = SNull            -- ^ @tag:yaml.org,2002:null@
            | SBool   !Bool    -- ^ @tag:yaml.org,2002:bool@
            | SFloat  !Double  -- ^ @tag:yaml.org,2002:float@
            | SInt    !Integer -- ^ @tag:yaml.org,2002:int@
            | SStr    !Text    -- ^ @tag:yaml.org,2002:str@

            | SUnknown !Tag !Text -- ^ unknown/unsupported tag or untagged (thus unresolved) scalar
            deriving (Eq,Ord,Show,Generic)

-- | @since 0.2.0
instance NFData Scalar where
  rnf SNull          = ()
  rnf (SBool _)      = ()
  rnf (SFloat _)     = ()
  rnf (SInt _)       = ()
  rnf (SStr _)       = ()
  rnf (SUnknown t _) = rnf t

-- | Definition of a [YAML 1.2 Schema](http://yaml.org/spec/1.2/spec.html#Schema)
--
-- A YAML schema defines how implicit tags are resolved to concrete tags and how data is represented textually in YAML.
data SchemaResolver = SchemaResolver
     { schemaResolverScalar            :: Tag -> YE.ScalarStyle -> T.Text -> Either String Scalar
     , schemaResolverSequence          :: Tag -> Either String Tag
     , schemaResolverMapping           :: Tag -> Either String Tag
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

-- | Some tags specified in YAML 1.2
tagNull, tagBool, tagStr, tagInt, tagFloat, tagSeq, tagMap, tagBang :: Tag
tagNull  = mkTag "tag:yaml.org,2002:null"
tagStr   = mkTag "tag:yaml.org,2002:str"
tagInt   = mkTag "tag:yaml.org,2002:int"
tagFloat = mkTag "tag:yaml.org,2002:float"
tagBool  = mkTag "tag:yaml.org,2002:bool"
tagSeq   = mkTag "tag:yaml.org,2002:seq"
tagMap   = mkTag "tag:yaml.org,2002:map"
tagBang  = mkTag "!"


-- | @since 0.2.0
data SchemaEncoder = SchemaEncoder
    { schemaEncoderScalar   :: Scalar -> Either String (Tag, ScalarStyle, T.Text)
    , schemaEncoderSequence :: Tag -> Either String Tag
    , schemaEncoderMapping  :: Tag -> Either String Tag
    }

mappingTag :: Tag -> Either String Tag
mappingTag t
  | t == tagMap  = Right untagged
  | otherwise    = Right t

seqTag :: Tag -> Either String Tag
seqTag t
  | t == tagSeq  = Right untagged
  | otherwise    = Right t


-- | \"Failsafe\" schema encoder as specified
-- in [YAML 1.2 / 10.1.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2803036)
--
-- @since 0.2.0
failsafeSchemaEncoder :: SchemaEncoder
failsafeSchemaEncoder = SchemaEncoder{..}
  where

    schemaEncoderScalar s = case s of
      SNull        -> Left  "SNull scalar type not supported in failsafeSchemaEncoder"
      SBool  _     -> Left  "SBool scalar type not supported in failsafeSchemaEncoder"
      SFloat _     -> Left  "SFloat scalar type not supported in failsafeSchemaEncoder"
      SInt   _     -> Left  "SInt scalar type not supported in failsafeSchemaEncoder"
      SStr   text  -> failEncodeStr text
      SUnknown t v -> Right (t, DoubleQuoted, v)

    schemaEncoderMapping  = mappingTag
    schemaEncoderSequence = seqTag

-- | Strict JSON schema encoder as specified
-- in [YAML 1.2 / 10.2.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2804356)
--
-- @since 0.2.0
jsonSchemaEncoder :: SchemaEncoder
jsonSchemaEncoder = SchemaEncoder{..}
  where

    schemaEncoderScalar s = case s of
      SNull         -> Right (untagged, Plain, "null")
      SBool  bool   -> Right (untagged, Plain, encodeBool bool)
      SFloat double -> Right (untagged, Plain, encodeDouble double)
      SInt   int    -> Right (untagged, Plain, encodeInt int)
      SStr   text   -> jsonEncodeStr text
      SUnknown _ _  -> Left  "SUnknown scalar type not supported in jsonSchemaEncoder"

    schemaEncoderMapping  = mappingTag
    schemaEncoderSequence = seqTag

-- | Core schema encoder as specified
-- in [YAML 1.2 / 10.3.2. Tag Resolution](http://yaml.org/spec/1.2/spec.html#id2805071)
--
-- @since 0.2.0
coreSchemaEncoder :: SchemaEncoder
coreSchemaEncoder = SchemaEncoder{..}
  where

    schemaEncoderScalar s = case s of
      SNull         -> Right (untagged, Plain, "null")
      SBool  bool   -> Right (untagged, Plain, encodeBool bool)
      SFloat double -> Right (untagged, Plain, encodeDouble double)
      SInt   int    -> Right (untagged, Plain, encodeInt int)
      SStr   text   -> coreEncodeStr text
      SUnknown t v  -> Right (t, DoubleQuoted, v)

    schemaEncoderMapping  = mappingTag
    schemaEncoderSequence = seqTag

-- | Encode Boolean
--
-- @since 0.2.0
encodeBool :: Bool -> T.Text
encodeBool b = if b then "true" else "false"

-- | Encode Double
--
-- @since 0.2.0
encodeDouble :: Double -> T.Text
encodeDouble d
  | d /= d      = ".nan"
  | d == (1/0)  = ".inf"
  | d == (-1/0) = "-.inf"
  | otherwise   = T.pack . show $ d

-- | Encode Integer
--
-- @since 0.2.0
encodeInt :: Integer -> T.Text
encodeInt = T.pack . show


failEncodeStr :: T.Text -> Either String (Tag, ScalarStyle, T.Text)
failEncodeStr t
  | T.isPrefixOf " " t               = Right (untagged, DoubleQuoted, t)
  | T.last t == ' '                  = Right (untagged, DoubleQuoted, t)
  | T.any (not. isPlainChar) t       = Right (untagged, DoubleQuoted, t)
  | otherwise                        = Right (untagged, Plain, t)

jsonEncodeStr :: T.Text -> Either String (Tag, ScalarStyle, T.Text)
jsonEncodeStr t
  | T.null t                         = Right (untagged, DoubleQuoted, t)
  | T.isPrefixOf " " t               = Right (untagged, DoubleQuoted, t)
  | T.last t == ' '                  = Right (untagged, DoubleQuoted, t)
  | T.any (not. isPlainChar) t       = Right (untagged, DoubleQuoted, t)
  | isAmbiguous jsonSchemaResolver t = Right (untagged, DoubleQuoted, t)
  | otherwise                        = Right (untagged, Plain, t)

coreEncodeStr :: T.Text -> Either String (Tag, ScalarStyle, T.Text)
coreEncodeStr t
  | T.null t                         = Right (untagged, DoubleQuoted, t)
  | T.isPrefixOf " " t               = Right (untagged, DoubleQuoted, t)
  | T.last t == ' '                  = Right (untagged, DoubleQuoted, t)
  | T.any (not. isPlainChar) t       = Right (untagged, DoubleQuoted, t)
  | isAmbiguous coreSchemaResolver t = Right (untagged, DoubleQuoted, t)
  | otherwise                        = Right (untagged, Plain, t)

-- | These are some characters which can be used in 'Plain' 'Scalar's safely without any quotes (see <https://yaml.org/spec/1.2/spec.html#c-indicator Indicator Characters>).
--
-- __NOTE__: This does not mean that other characters (like @"\\n"@ and other special characters like @"-?:,[]{}#&*!,>%\@`\"\'"@) cannot be used in 'Plain' 'Scalar's.
--
-- @since 0.2.0
isPlainChar :: Char -> Bool
isPlainChar c = C.isAlphaNum c || c `elem` (" ~$^+=</;._\\" :: String)  -- not $ c `elem` "\n-?:,[]{}#&*!,>%@`\\'\""

-- | Returns True if the string can be decoded by the given 'SchemaResolver'
-- into a 'Scalar' which is not a of type 'SStr'.
--
-- >>> isAmbiguous coreSchemaResolver "true"
-- True
--
-- >>> isAmbiguous failSchemaResolver "true"
-- False
--
-- @since 0.2.0
isAmbiguous :: SchemaResolver -> T.Text -> Bool
isAmbiguous SchemaResolver{..} t = case schemaResolverScalar untagged Plain t of
  Left err        -> error err
  Right (SStr _ ) -> False
  Right _         -> True

-- | According to YAML 1.2 'coreSchemaEncoder' is the default 'SchemaEncoder'
--
-- By default, 'Scalar's are encoded as follows:
--
-- * String which are made of Plain Characters (see 'isPlainChar'), unambiguous (see 'isAmbiguous') and do not contain any leading/trailing spaces are encoded as 'Plain' 'Scalar'.
--
-- * Rest of the strings are encoded in DoubleQuotes
--
-- * Booleans are encoded using 'encodeBool'
--
-- * Double values are encoded using 'encodeDouble'
--
-- * Integral values are encoded using 'encodeInt'
--
-- @since 0.2.0
defaultSchemaEncoder :: SchemaEncoder
defaultSchemaEncoder = coreSchemaEncoder

-- | Set the 'Scalar' style in the encoded YAML. This is a function that decides
-- for each 'Scalar' the type of YAML string to output.
--
-- __WARNING__: You must ensure that special strings (like @"true"@\/@"false"@\/@"null"@\/@"1234"@) are not encoded with the 'Plain' style, because
-- then they will be decoded as boolean, null or numeric values. You can use 'isAmbiguous' to detect them.
--
-- __NOTE__: For different 'SchemaResolver's, different strings are ambiguous. For example, @"true"@ is not ambiguous for 'failsafeSchemaResolver'.
--
-- @since 0.2.0
setScalarStyle :: (Scalar -> Either String (Tag, ScalarStyle, T.Text)) -> SchemaEncoder -> SchemaEncoder
setScalarStyle customScalarEncoder encoder = encoder { schemaEncoderScalar = customScalarEncoder }

