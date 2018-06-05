{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-3.0
--
-- Document oriented [YAML](http://yaml.org/spec/1.2/spec.html) parsing API inspired by [aeson](http://hackage.haskell.org/package/aeson).
--
-- === Overview
--
-- The diagram below depicts the standard layers of a [YAML 1.2](http://yaml.org/spec/1.2/spec.html) processor. This module covers the upper /Native/ and /Representation/ layers, whereas the "Data.YAML.Event" and "Data.YAML.Token" modules provide access to the lower /Serialization/ and /Presentation/ layers respectively.
--
-- <<http://yaml.org/spec/1.2/overview2.png>>
--
-- === Quick Start Tutorial
--
-- Let's assume we want to decode (i.e. /load/) a simple YAML document
--
-- > - name: Erik Weisz
-- >   age: 52
-- >   magic: True
-- > - name: Mina Crandon
-- >   age: 53
--
-- into a native Haskell data structure of type @[Person]@, i.e. a list of 'Person' records.
--
-- The code below shows how to manually define a @Person@ record type together with a 'FromYAML' instance:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.YAML
-- >
-- > data Person = Person
-- >     { name  :: Text
-- >     , age   :: Int
-- >     , magic :: Bool
-- >     } deriving Show
-- >
-- > instance FromYAML Person where
-- >    parseYAML = withMap "Person" $ \m -> Person
-- >        <$> m .: "name"
-- >        <*> m .: "age"
-- >        <*> m .:? "magic" .!= False
--
-- And now we can 'decode' the YAML document like so:
--
-- >>> decode "- name: Erik Weisz\n  age: 52\n  magic: True\n- name: Mina Crandon\n  age: 53" :: Either String [[Person]]
-- Right [[Person {name = "Erik Weisz", age = 52, magic = True},Person {name = "Mina Crandon", age = 53, magic = False}]]
--
--
module Data.YAML
    (
      -- * Typeclass-based resolving/decoding
      decode
    , decodeStrict
    , FromYAML(..)
    , Parser
    , parseEither
    , typeMismatch

      -- ** Accessors for YAML 'Mapping's
    , Mapping
    , (.:), (.:?), (.:!), (.!=)

      -- ** Prism-style parsers
    , withSeq
    , withBool
    , withFloat
    , withInt
    , withNull
    , withStr
    , withMap

      -- * \"Concrete\" AST
    , decodeNode
    , decodeNode'
    , Doc(..)
    , Node(..)
    , Scalar(..)

      -- * YAML 1.2 Schema resolvers
    , SchemaResolver(..)
    , failsafeSchemaResolver
    , jsonSchemaResolver
    , coreSchemaResolver

      -- * Generalised AST construction
    , decodeLoader
    , Loader(..)
    , NodeId

    ) where

import qualified Control.Monad.Fail   as Fail
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Map             as Map
import qualified Data.Text            as T

import           Data.YAML.Event      (Tag, isUntagged, tagToText)
import           Data.YAML.Loader
import           Data.YAML.Schema

import           Util

-- | YAML Document tree/graph
newtype Doc n = Doc n deriving (Eq,Ord,Show)

-- | YAML Document node
data Node = Scalar   !Scalar
          | Mapping  !Tag Mapping
          | Sequence !Tag [Node]
          | Anchor   !NodeId !Node
          deriving (Eq,Ord,Show)

-- | YAML mapping
type Mapping = Map Node Node

-- | Retrieve value in 'Mapping' indexed by a @!!str@ 'Text' key.
--
-- This parser fails if the key doesn't exist.
(.:) :: FromYAML a => Mapping -> Text -> Parser a
m .: k = maybe (fail $ "key " ++ show k ++ " not found") parseYAML (Map.lookup (Scalar (SStr k)) m)

-- | Retrieve optional value in 'Mapping' indexed by a @!!str@ 'Text' key.
--
-- 'Nothing' is returned if the key is missing or points to a @tag:yaml.org,2002:null@ node.
-- This combinator only fails if the key exists but cannot be converted to the required type.
--
-- See also '.:!'.
(.:?) :: FromYAML a => Mapping -> Text -> Parser (Maybe a)
m .:? k = maybe (pure Nothing) parseYAML (Map.lookup (Scalar (SStr k)) m)

-- | Retrieve optional value in 'Mapping' indexed by a @!!str@ 'Text' key.
--
-- 'Nothing' is returned if the key is missing.
-- This combinator only fails if the key exists but cannot be converted to the required type.
--
-- __NOTE__: This is a variant of '.:?' which doesn't map a @tag:yaml.org,2002:null@ node to 'Nothing'.
(.:!) :: FromYAML a => Mapping -> Text -> Parser (Maybe a)
m .:! k = maybe (pure Nothing) (fmap Just . parseYAML) (Map.lookup (Scalar (SStr k)) m)

-- | Defaulting helper to be used with '.:?' or '.:!'.
(.!=) :: Parser (Maybe a) -> a -> Parser a
mv .!= def = fmap (maybe def id) mv


-- | Parse and decode YAML document(s) into 'Node' graphs
--
-- This is a convenience wrapper over `decodeNode'`
--
-- > decodeNode = decodeNode' coreSchemaResolver False False
--
-- In other words,
--
-- * Use the YAML 1.2 Core schema for resolving
-- * Don't create 'Anchor' nodes
-- * Disallow cyclic anchor references
--
decodeNode :: BS.L.ByteString -> Either String [Doc Node]
decodeNode = decodeNode' coreSchemaResolver False False


-- | Customizable variant of 'decodeNode'
--
decodeNode' :: SchemaResolver  -- ^ YAML Schema resolver to use
            -> Bool            -- ^ Whether to emit anchor nodes
            -> Bool            -- ^ Whether to allow cyclic references
            -> BS.L.ByteString -- ^ YAML document to parse
            -> Either String [Doc Node]
decodeNode' SchemaResolver{..} anchorNodes allowCycles bs0
  = map Doc <$> runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t s v -> pure $ fmap Scalar (schemaResolverScalar t s v)
                            , ySequence = \t vs  -> pure $ schemaResolverSequence t >>= \t' -> Right (Sequence t' vs)
                            , yMapping  = \t kvs -> pure $ schemaResolverMapping  t >>= \t' -> Right (Mapping t' (Map.fromList kvs))
                            , yAlias    = if allowCycles
                                          then \_ _ n -> pure $ Right n
                                          else \_ c n -> pure $ if c then Left "cycle detected" else Right n
                            , yAnchor   = if anchorNodes
                                          then \j n   -> pure $ Right (Anchor j n)
                                          else \_ n   -> pure $ Right n
                            }


----------------------------------------------------------------------------

-- | YAML Parser 'Monad' used by 'FromYAML'
--
-- See also 'parseEither' or 'decode'
newtype Parser a = P { unP :: Either String a }

instance Functor Parser where
  fmap f (P x) = P (fmap f x)

  x <$ P (Right _) = P (Right x)
  _ <$ P (Left e)  = P (Left e)

instance Applicative Parser where
  pure = P . Right

  P (Left e)  <*> _   = P (Left e)
  P (Right f) <*> P r = P (fmap f r)

  P (Left e)   *> _   = P (Left e)
  P (Right _)  *> p   = p

instance Monad Parser where
  return = pure
  P m >>= k = P (m >>= unP . k)
  (>>) = (*>)
  fail = Fail.fail

-- | @since 0.1.1.0
instance Fail.MonadFail Parser where
  fail = P . Left

-- | @since 0.1.1.0
instance Alternative Parser where
  empty = fail "empty"

  P (Left _) <|> y = y
  x          <|> _ = x

-- | @since 0.1.1.0
instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- | Run 'Parser'
--
-- A common use-case is 'parseEither' 'parseYAML'.
parseEither :: Parser a -> Either String a
parseEither = unP

-- | Informative failure helper
--
-- This is typically used in fall-through cases of 'parseYAML' like so
--
-- > instance FromYAML ... where
-- >   parseYAML ...  = ...
-- >   parseYAML node = typeMismatch "SomeThing" node
--
-- @since 0.1.1.0
typeMismatch :: String   -- ^ descriptive name of expected data
             -> Node     -- ^ actual node
             -> Parser a
typeMismatch expected node = fail ("expected " ++ expected ++ " instead of " ++ got)
  where
    got = case node of
            Scalar (SBool _)             -> "!!bool"
            Scalar (SInt _)              -> "!!int"
            Scalar  SNull                -> "!!null"
            Scalar (SStr _)              -> "!!str"
            Scalar (SFloat _)            -> "!!float"
            Scalar (SUnknown t v)
              | isUntagged t             -> tagged t ++ show v
              | otherwise                -> "(unsupported) " ++ tagged t ++ "scalar"
            (Anchor _ _)                 -> "anchor"
            (Mapping t _)                -> tagged t ++ " mapping"
            (Sequence t _)               -> tagged t ++ " sequence"

    tagged t0 = case tagToText t0 of
               Nothing -> "non-specifically ? tagged (i.e. unresolved) "
               Just t  -> T.unpack t ++ " tagged"

-- | A type into which YAML nodes can be converted/deserialized
class FromYAML a where
  parseYAML :: Node -> Parser a

-- | Operate on @tag:yaml.org,2002:null@ node (or fail)
withNull :: String -> Parser a -> Node -> Parser a
withNull _        f (Scalar SNull) = f
withNull expected _ v              = typeMismatch expected v


-- | Trivial instance
instance FromYAML Node where
  parseYAML = pure

instance FromYAML Bool where
  parseYAML = withBool "!!bool" pure

-- | Operate on @tag:yaml.org,2002:bool@ node (or fail)
withBool :: String -> (Bool -> Parser a) -> Node -> Parser a
withBool _        f (Scalar (SBool b)) = f b
withBool expected _ v                  = typeMismatch expected v

instance FromYAML Text where
  parseYAML = withStr "!!str" pure

-- | Operate on @tag:yaml.org,2002:str@ node (or fail)
withStr :: String -> (Text -> Parser a) -> Node -> Parser a
withStr _        f (Scalar (SStr b)) = f b
withStr expected _ v                 = typeMismatch expected v

instance FromYAML Integer where
  parseYAML = withInt "!!int" pure

-- | Operate on @tag:yaml.org,2002:int@ node (or fail)
withInt :: String -> (Integer -> Parser a) -> Node -> Parser a
withInt _        f (Scalar (SInt b)) = f b
withInt expected _ v                 = typeMismatch expected v

-- | @since 0.1.0.0
instance FromYAML Natural where
  parseYAML = withInt "!!int" $ \b -> if b < 0 then fail ("!!int " ++ show b ++ " out of range for 'Natural'")
                                               else pure (fromInteger b)

-- helper for fixed-width integers
{-# INLINE parseInt #-}
parseInt :: (Integral a, Bounded a) => [Char] -> Node -> Parser a
parseInt name = withInt "!!int" $ \b -> maybe (fail $ "!!int " ++ show b ++ " out of range for '" ++ name ++ "'") pure $
                                        fromIntegerMaybe b

instance FromYAML Int    where parseYAML = parseInt "Int"
instance FromYAML Int8   where parseYAML = parseInt "Int8"
instance FromYAML Int16  where parseYAML = parseInt "Int16"
instance FromYAML Int32  where parseYAML = parseInt "Int32"
instance FromYAML Int64  where parseYAML = parseInt "Int64"
instance FromYAML Word   where parseYAML = parseInt "Word"
instance FromYAML Word8  where parseYAML = parseInt "Word8"
instance FromYAML Word16 where parseYAML = parseInt "Word16"
instance FromYAML Word32 where parseYAML = parseInt "Word32"
instance FromYAML Word64 where parseYAML = parseInt "Word64"


instance FromYAML Double where
  parseYAML = withFloat "!!float" pure

-- | Operate on @tag:yaml.org,2002:float@ node (or fail)
withFloat :: String -> (Double -> Parser a) -> Node -> Parser a
withFloat _        f (Scalar (SFloat b)) = f b
withFloat expected _ v                   = typeMismatch expected v


instance (Ord k, FromYAML k, FromYAML v) => FromYAML (Map k v) where
  parseYAML = withMap "!!map" $ \xs -> Map.fromList <$> mapM (\(a,b) -> (,) <$> parseYAML a <*> parseYAML b) (Map.toList xs)

-- | Operate on @tag:yaml.org,2002:seq@ node (or fail)
withMap :: String -> (Mapping -> Parser a) -> Node -> Parser a
withMap _        f (Mapping tag xs)
  | tag == tagMap    = f xs
withMap expected _ v = typeMismatch expected v

instance FromYAML v => FromYAML [v] where
  parseYAML = withSeq "!!seq" (mapM parseYAML)

-- | Operate on @tag:yaml.org,2002:seq@ node (or fail)
withSeq :: String -> ([Node] -> Parser a) -> Node -> Parser a
withSeq _        f (Sequence tag xs)
  | tag == tagSeq    = f xs
withSeq expected _ v = typeMismatch expected v

instance FromYAML a => FromYAML (Maybe a) where
  parseYAML (Scalar SNull) = pure Nothing
  parseYAML j              = Just <$> parseYAML j

----------------------------------------------------------------------------

instance (FromYAML a, FromYAML b) => FromYAML (a,b) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b] -> (,) <$> parseYAML a
                                          <*> parseYAML b
                             _     -> fail ("expected 2-sequence but got " ++ show (length xs) ++ "-sequence instead")

instance (FromYAML a, FromYAML b, FromYAML c) => FromYAML (a,b,c) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b,c] -> (,,) <$> parseYAML a
                                             <*> parseYAML b
                                             <*> parseYAML c
                             _     -> fail ("expected 3-sequence but got " ++ show (length xs) ++ "-sequence instead")


instance (FromYAML a, FromYAML b, FromYAML c, FromYAML d) => FromYAML (a,b,c,d) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b,c,d] -> (,,,) <$> parseYAML a
                                                <*> parseYAML b
                                                <*> parseYAML c
                                                <*> parseYAML d
                             _     -> fail ("expected 4-sequence but got " ++ show (length xs) ++ "-sequence instead")


instance (FromYAML a, FromYAML b, FromYAML c, FromYAML d, FromYAML e) => FromYAML (a,b,c,d,e) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b,c,d,e] -> (,,,,) <$> parseYAML a
                                                   <*> parseYAML b
                                                   <*> parseYAML c
                                                   <*> parseYAML d
                                                   <*> parseYAML e
                             _     -> fail ("expected 5-sequence but got " ++ show (length xs) ++ "-sequence instead")


instance (FromYAML a, FromYAML b, FromYAML c, FromYAML d, FromYAML e, FromYAML f) => FromYAML (a,b,c,d,e,f) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b,c,d,e,f] -> (,,,,,) <$> parseYAML a
                                                      <*> parseYAML b
                                                      <*> parseYAML c
                                                      <*> parseYAML d
                                                      <*> parseYAML e
                                                      <*> parseYAML f
                             _     -> fail ("expected 6-sequence but got " ++ show (length xs) ++ "-sequence instead")


instance (FromYAML a, FromYAML b, FromYAML c, FromYAML d, FromYAML e, FromYAML f, FromYAML g) => FromYAML (a,b,c,d,e,f,g) where
  parseYAML = withSeq "!!seq" $ \xs ->
                           case xs of
                             [a,b,c,d,e,f,g] -> (,,,,,,) <$> parseYAML a
                                                         <*> parseYAML b
                                                         <*> parseYAML c
                                                         <*> parseYAML d
                                                         <*> parseYAML e
                                                         <*> parseYAML f
                                                         <*> parseYAML g
                             _     -> fail ("expected 7-sequence but got " ++ show (length xs) ++ "-sequence instead")


-- | Decode YAML document(s) using the YAML 1.2 Core schema
--
-- Each document contained in the YAML stream produce one element of
-- the response list. Here's an example of decoding two concatenated
-- YAML documents:
--
-- >>> decode "Foo\n---\nBar" :: Either String [Text]
-- Right ["Foo","Bar"]
--
-- Note that an empty stream doesn't contain any (non-comment)
-- document nodes, and therefore results in an empty result list:
--
-- >>> decode "# just a comment" :: Either String [Text]
-- Right []
--
-- 'decode' uses the same settings as 'decodeNode' for tag-resolving. If
-- you need a different custom parsing configuration, you need to
-- combine 'parseEither' and `decodeNode'` yourself.
decode :: FromYAML v => BS.L.ByteString -> Either String [v]
decode bs0 = decodeNode bs0 >>= mapM (parseEither . parseYAML . (\(Doc x) -> x))

-- | Like 'decode' but takes a strict 'BS.ByteString'
--
-- @since 0.1.1.0
decodeStrict :: FromYAML v => BS.ByteString -> Either String [v]
decodeStrict = decode . BS.L.fromChunks . (:[])
