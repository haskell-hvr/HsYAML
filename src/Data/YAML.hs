{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Safe              #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Document oriented [YAML](http://yaml.org/spec/1.2/spec.html) parsing API inspired by [aeson](http://hackage.haskell.org/package/aeson).

module Data.YAML
    (

      -- * Overview
      -- $overview

      -- * Quick Start Tutorial
      -- $start

      -- ** Decoding/Loading YAML document
      -- $loading

      -- ** Encoding/dumping
      -- $dumping

      -- * Typeclass-based resolving/decoding
      decode
    , decode1
    , decodeStrict
    , decode1Strict
    , FromYAML(..)
    , Parser
    , parseEither
    , failAtNode
    , typeMismatch

      -- ** Accessors for YAML t'Mapping's
    , Mapping
    , (.:), (.:?), (.:!), (.!=)

      -- * Typeclass-based dumping
    , encode
    , encode1
    , encodeStrict
    , encode1Strict
    , ToYAML(..)

      -- ** Accessors for encoding t'Mapping's
    , Pair
    , mapping
    , (.=)

      -- ** Prism-style parsers
    , withScalar
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
    , encodeNode
    , encodeNode'
    , Doc(Doc,docRoot)
    , Node(..)
    , Scalar(..)

      -- * Source locations
    , Pos(..)
    , prettyPosWithSource

      -- * YAML 1.2 Schema resolvers
      --
      -- | See also "Data.YAML.Schema"
    , SchemaResolver
    , failsafeSchemaResolver
    , jsonSchemaResolver
    , coreSchemaResolver

      -- * YAML 1.2 Schema encoders
      --
      -- | See also "Data.YAML.Schema"
    , SchemaEncoder
    , failsafeSchemaEncoder
    , jsonSchemaEncoder
    , coreSchemaEncoder

      -- * Generalised AST construction
    , decodeLoader
    , Loader(..)
    , LoaderT
    , NodeId

    ) where

import qualified Control.Monad.Fail        as Fail
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.Map                  as Map
import qualified Data.Text                 as T

import           Data.YAML.Dumper
import           Data.YAML.Event           (isUntagged, tagToText)
import           Data.YAML.Internal
import           Data.YAML.Loader
import           Data.YAML.Pos
import           Data.YAML.Schema.Internal

import           Util

-- $overview
--
-- The diagram below depicts the standard layers of a [YAML 1.2](http://yaml.org/spec/1.2/spec.html) processor. This module covers the upper /Native/ and /Representation/ layers, whereas the "Data.YAML.Event" and "Data.YAML.Token" modules provide access to the lower /Serialization/ and /Presentation/ layers respectively.
--
-- <<http://yaml.org/spec/1.2/overview2.png>>
--
-- $start
--
-- This section contains basic information on the different ways to work with YAML data using this library.
--
-- $loading
--
-- We address the process of loading data from a YAML document as decoding.
--
-- Let's assume we want to decode (i.e. /load/) a simple YAML document
--
-- > - name: Erik Weisz
-- >   age: 52
-- >   magic: True
-- > - name: Mina Crandon
-- >   age: 53
--
-- into a native Haskell data structure of type @[Person]@, i.e. a list of @Person@ records.
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
-- >>> decode "- name: Erik Weisz\n  age: 52\n  magic: True\n- name: Mina Crandon\n  age: 53" :: Either (Pos,String) [[Person]]
-- Right [[Person {name = "Erik Weisz", age = 52, magic = True},Person {name = "Mina Crandon", age = 53, magic = False}]]
--
-- There are predefined 'FromYAML' instance for many types.
--
-- The example below shows decoding multiple YAML documents into a list of 'Int' lists:
--
-- >>> decode "---\n- 1\n- 2\n- 3\n---\n- 4\n- 5\n- 6" :: Either (Pos,String) [[Int]]
-- Right [[1,2,3],[4,5,6]]
--
-- If you are expecting exactly one YAML document then you can use convenience function 'decode1'
--
-- >>> decode1 "- 1\n- 2\n- 3\n" :: Either (Pos,String) [Int]
-- Right [1,2,3]
--
-- == Working with AST
--
-- Sometimes we want to work with YAML data directly, without first converting it to a custom data type.
--
-- We can easily do that by using the 'Node' type, which is an instance of 'FromYAML', is used to represent an arbitrary YAML AST (abstract syntax tree). For example,
--
-- >>> decode1 "Name: Vijay" :: Either (Pos,String) (Node Pos)
-- Right (Mapping (Pos {posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0}) Just "tag:yaml.org,2002:map" (fromList [(Scalar (Pos {posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0}) (SStr "Name"),Scalar (Pos {posByteOffset = 6, posCharOffset = 6, posLine = 1, posColumn = 6}) (SStr "Vijay"))]))
--
-- The type parameter 'Pos' is used to indicate the position of each YAML 'Node' in the document.
-- So using the 'Node' type we can easily decode any YAML document.
--
-- == Pretty-printing source locations
--
-- Syntax errors or even conversion errors are reported with a source location, e.g.
--
-- >>> decode "- name: Erik Weisz\n  age: 52\n  magic: True\n- name: Mina Crandon\n  age: young" :: Either (Pos,String) [[Person]]
-- Left (Pos {posByteOffset = 71, posCharOffset = 71, posLine = 5, posColumn = 7},"expected !!int instead of !!str")
--
-- While accurate this isn't a very convenient error representation. Instead we can use the 'prettyPosWithSource' helper function to create more convenient error report like so
--
-- @
-- readPersons :: FilePath -> IO [Person]
-- readPersons fname = do
--    raw <- BS.L.readFile fname
--    case 'decode1' raw of
--      Left (loc,emsg) -> do
--        hPutStrLn stderr (fname ++ ":" ++ 'prettyPosWithSource' loc raw " error" ++ emsg)
--        pure []
--      Right persons -> pure persons
-- @
--
-- which will then print errors in a common form such as
--
-- > people.yaml:5:7: error
-- >    |
-- >  5 |   age: young
-- >    |        ^
-- > expected !!int instead of !!str
--


-- | Retrieve value in t'Mapping' indexed by a @!!str@ 'Text' key.
--
-- This parser fails if the key doesn't exist.
(.:) :: FromYAML a => Mapping Pos -> Text -> Parser a
m .: k = maybe (fail $ "key " ++ show k ++ " not found") parseYAML (Map.lookup (Scalar fakePos (SStr k)) m)

-- | Retrieve optional value in t'Mapping' indexed by a @!!str@ 'Text' key.
--
-- 'Nothing' is returned if the key is missing or points to a @tag:yaml.org,2002:null@ node.
-- This combinator only fails if the key exists but cannot be converted to the required type.
--
-- See also '.:!'.
(.:?) :: FromYAML a => Mapping Pos -> Text -> Parser (Maybe a)
m .:? k = maybe (pure Nothing) parseYAML (Map.lookup (Scalar fakePos (SStr k)) m)

-- | Retrieve optional value in t'Mapping' indexed by a @!!str@ 'Text' key.
--
-- 'Nothing' is returned if the key is missing.
-- This combinator only fails if the key exists but cannot be converted to the required type.
--
-- __NOTE__: This is a variant of '.:?' which doesn't map a @tag:yaml.org,2002:null@ node to 'Nothing'.
(.:!) :: FromYAML a => Mapping Pos -> Text -> Parser (Maybe a)
m .:! k = maybe (pure Nothing) (fmap Just . parseYAML) (Map.lookup (Scalar fakePos (SStr k)) m)

-- | Defaulting helper to be used with '.:?' or '.:!'.
(.!=) :: Parser (Maybe a) -> a -> Parser a
mv .!= def = fmap (maybe def id) mv

fakePos :: Pos
fakePos = Pos { posByteOffset = -1 , posCharOffset = -1  , posLine = 1 , posColumn = 0 }

-- | Parse and decode YAML document(s) into 'Node' graphs
--
-- This is a convenience wrapper over `decodeNode'`, i.e.
--
-- @
-- decodeNode = decodeNode' 'coreSchemaResolver' False False
-- @
--
-- In other words,
--
-- * Use the YAML 1.2 Core schema for resolving
-- * Don't create 'Anchor' nodes
-- * Disallow cyclic anchor references
--
-- @since 0.2.0
--
decodeNode :: BS.L.ByteString -> Either (Pos, String) [Doc (Node Pos)]
decodeNode = decodeNode' coreSchemaResolver False False


-- | Customizable variant of 'decodeNode'
--
-- @since 0.2.0
--
decodeNode' :: SchemaResolver  -- ^ YAML Schema resolver to use
            -> Bool            -- ^ Whether to emit anchor nodes
            -> Bool            -- ^ Whether to allow cyclic references
            -> BS.L.ByteString -- ^ YAML document to parse
            -> Either (Pos, String) [Doc (Node Pos)]
decodeNode' SchemaResolver{..} anchorNodes allowCycles bs0
  = map Doc <$> runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t s v pos-> pure $ case schemaResolverScalar t s v of
                                                                Left  e  -> Left (pos,e)
                                                                Right v' -> Right (Scalar pos v')
                            , ySequence = \t vs pos -> pure $ case schemaResolverSequence t of
                                                                Left  e  -> Left (pos,e)
                                                                Right t' -> Right (Sequence pos t' vs)
                            , yMapping  = \t kvs pos-> pure $ case schemaResolverMapping  t of
                                                                Left  e  -> Left (pos,e)
                                                                Right t' -> Mapping pos t' <$> mkMap kvs
                            , yAlias    = if allowCycles
                                          then \_ _ n _-> pure $ Right n
                                          else \_ c n pos -> pure $ if c then Left (pos,"cycle detected") else Right n
                            , yAnchor   = if anchorNodes
                                          then \j n pos  -> pure $ Right (Anchor pos j n)
                                          else \_ n _  -> pure $ Right n
                            }

    mkMap :: [(Node Pos, Node Pos)] -> Either (Pos, String) (Map (Node Pos) (Node Pos))
    mkMap kvs
      | schemaResolverMappingDuplicates = Right $! Map.fromList kvs
      | otherwise = case mapFromListNoDupes kvs of
          Left (k,_) -> Left (nodeLoc k,"Duplicate key in mapping: " ++ show k)
          Right m    -> Right m

----------------------------------------------------------------------------

-- | YAML Parser 'Monad' used by 'FromYAML'
--
-- See also 'parseEither' or 'decode'
newtype Parser a = P { unP :: Either (Pos, String) a }

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
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif


-- | @since 0.1.1.0
--
-- __NOTE__: 'fail' doesn't convey proper position information unless used within the @with*@-style helpers; consequently it's recommended to use 'failAtNode' when /not/ covered by the location scope of a @with*@-style combinator.
instance Fail.MonadFail Parser where
  fail s = P (Left (fakePos, s))

-- | Trigger parsing failure located at a specific 'Node'
--
-- @since 0.2.0.0
failAtNode :: Node Pos -> String -> Parser a
failAtNode n s = P (Left (nodeLoc n, s))

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
parseEither :: Parser a -> Either (Pos, String) a
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
             -> Node Pos     -- ^ actual node
             -> Parser a
typeMismatch expected node = failAtNode node ("expected " ++ expected ++ " instead of " ++ got)
  where
    got = case node of
            Scalar _ (SBool _)             -> "!!bool"
            Scalar _ (SInt _)              -> "!!int"
            Scalar _  SNull                -> "!!null"
            Scalar _ (SStr _)              -> "!!str"
            Scalar _ (SFloat _)            -> "!!float"
            Scalar _ (SUnknown t v)
              | isUntagged t               -> tagged t ++ show v
              | otherwise                  -> "(unsupported) " ++ tagged t ++ "scalar"
            Anchor _ _ _                   -> "anchor"
            Mapping _ t _                  -> tagged t ++ " mapping"
            Sequence _ t _                 -> tagged t ++ " sequence"

    tagged t0 = case tagToText t0 of
               Nothing -> "non-specifically ? tagged (i.e. unresolved) "
               Just t  -> T.unpack t ++ " tagged"

-- | A type into which YAML nodes can be converted/deserialized
class FromYAML a where
  parseYAML :: Node Pos -> Parser a

-- This helper fixes up 'fakePos' locations to a better guess; this is
-- mostly used by the with*-style combinators
{-# INLINE fixupFailPos #-}
fixupFailPos :: Pos -> Parser a -> Parser a
fixupFailPos pos (P (Left (pos0,emsg)))
  | pos0 == fakePos  = P (Left (pos,emsg))
fixupFailPos _ p = p

-- | Operate on @tag:yaml.org,2002:null@ node (or fail)
withNull :: String -> Parser a -> Node Pos -> Parser a
withNull _        f (Scalar pos SNull) = fixupFailPos pos f
withNull expected _ v                  = typeMismatch expected v

-- | Operate on t'Scalar' node (or fail)
--
-- @since 0.2.1
withScalar :: String -> (Scalar -> Parser a) -> Node Pos -> Parser a
withScalar _        f (Scalar pos sca) = fixupFailPos pos (f sca)
withScalar expected _ v                = typeMismatch expected v

-- | Trivial instance
instance (loc ~ Pos) => FromYAML (Node loc) where
  parseYAML = pure

-- | @since 0.2.1
instance FromYAML Scalar where
  parseYAML = withScalar "scalar" pure

instance FromYAML Bool where
  parseYAML = withBool "!!bool" pure

-- | Operate on @tag:yaml.org,2002:bool@ node (or fail)
withBool :: String -> (Bool -> Parser a) -> Node Pos -> Parser a
withBool _        f (Scalar pos (SBool b)) = fixupFailPos pos (f b)
withBool expected _ v                      = typeMismatch expected v

instance FromYAML Text where
  parseYAML = withStr "!!str" pure

-- | Operate on @tag:yaml.org,2002:str@ node (or fail)
withStr :: String -> (Text -> Parser a) -> Node Pos -> Parser a
withStr _        f (Scalar pos (SStr b)) = fixupFailPos pos (f b)
withStr expected _ v                     = typeMismatch expected v

instance FromYAML Integer where
  parseYAML = withInt "!!int" pure

-- | Operate on @tag:yaml.org,2002:int@ node (or fail)
withInt :: String -> (Integer -> Parser a) -> Node Pos -> Parser a
withInt _        f (Scalar pos (SInt b)) = fixupFailPos pos (f b)
withInt expected _ v                     = typeMismatch expected v

-- | @since 0.1.1.0
instance FromYAML Natural where
  parseYAML = withInt "!!int" $ \b -> if b < 0 then fail ("!!int " ++ show b ++ " out of range for 'Natural'")
                                               else pure (fromInteger b)

-- helper for fixed-width integers
{-# INLINE parseInt #-}
parseInt :: (Integral a, Bounded a) => [Char] -> Node Pos -> Parser a
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
withFloat :: String -> (Double -> Parser a) -> Node Pos -> Parser a
withFloat _        f (Scalar pos (SFloat b)) = fixupFailPos pos (f b)
withFloat expected _ v                       = typeMismatch expected v


instance (Ord k, FromYAML k, FromYAML v) => FromYAML (Map k v) where
  parseYAML = withMap "!!map" $ \xs -> Map.fromList <$> mapM (\(a,b) -> (,) <$> parseYAML a <*> parseYAML b) (Map.toList xs)

-- | Operate on @tag:yaml.org,2002:map@ node (or fail)
withMap :: String -> (Mapping Pos -> Parser a) -> Node Pos -> Parser a
withMap _        f (Mapping pos tag xs)
  | tag == tagMap    = fixupFailPos pos (f xs)
withMap expected _ v = typeMismatch expected v

instance FromYAML v => FromYAML [v] where
  parseYAML = withSeq "!!seq" (mapM parseYAML)

-- | Operate on @tag:yaml.org,2002:seq@ node (or fail)
withSeq :: String -> ([Node Pos] -> Parser a) -> Node Pos-> Parser a
withSeq _        f (Sequence pos tag xs)
  | tag == tagSeq    = fixupFailPos pos (f xs)
withSeq expected _ v = typeMismatch expected v

instance FromYAML a => FromYAML (Maybe a) where
  parseYAML (Scalar _ SNull) = pure Nothing
  parseYAML j                = Just <$> parseYAML j

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
-- >>> decode "Foo\n---\nBar" :: Either (Pos,String) [Text]
-- Right ["Foo","Bar"]
--
-- Note that an empty stream doesn't contain any (non-comment)
-- document nodes, and therefore results in an empty result list:
--
-- >>> decode "# just a comment" :: Either (Pos,String) [Text]
-- Right []
--
-- 'decode' uses the same settings as 'decodeNode' for tag-resolving. If
-- you need a different custom parsing configuration, you need to
-- combine 'parseEither' and `decodeNode'` yourself.
--
-- The 'decode' as well as the 'decodeNode' functions supports
-- decoding from YAML streams using the UTF-8, UTF-16 (LE or BE), or
-- UTF-32 (LE or BE) encoding (which is auto-detected).
--
-- @since 0.2.0
--
decode :: FromYAML v => BS.L.ByteString -> Either (Pos, String) [v]
decode bs0 = decodeNode bs0 >>= mapM (parseEither . parseYAML . (\(Doc x) -> x))

-- | Convenience wrapper over 'decode' expecting exactly one YAML document
--
-- >>> decode1 "---\nBar\n..." :: Either (Pos,String) Text
-- Right "Bar"
--
-- >>> decode1 "Foo\n---\nBar" :: Either (Pos,String) Text
-- Left (Pos {posByteOffset = 8, posCharOffset = 8, posLine = 3, posColumn = 0},"unexpected multiple YAML documents")
--
-- >>> decode1 "# Just a comment" :: Either (Pos,String) Text
-- Left (Pos {posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0},"empty YAML stream")
--
-- @since 0.2.0
--
decode1 :: FromYAML v => BS.L.ByteString -> Either (Pos, String) v
decode1 bs0 = do
  docs <- decodeNode bs0
  case docs of
    []  -> Left (Pos { posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0 }, "empty YAML stream")
    [Doc v] -> parseEither $ parseYAML $ v
    (_:Doc n:_) -> Left (nodeLoc n, "unexpected multiple YAML documents")

-- | Like 'decode' but takes a strict 'BS.ByteString'
--
-- @since 0.2.0
--
decodeStrict :: FromYAML v => BS.ByteString -> Either (Pos, String) [v]
decodeStrict = decode . BS.L.fromChunks . (:[])

-- | Like 'decode1' but takes a strict 'BS.ByteString'
--
-- @since 0.2.0
--
decode1Strict :: FromYAML v => BS.ByteString -> Either (Pos, String) v
decode1Strict = decode1 . BS.L.fromChunks . (:[])

-- $dumping
--
-- We address the process of dumping information from a Haskell-data type(s) to a YAML document(s) as encoding.
--
-- Suppose we want to 'encode' a Haskell-data type Person
--
-- @
-- data Person = Person
--     { name :: Text
--     , age  :: Int
--     } deriving Show
-- @
--
-- To 'encode' data, we need to define a 'ToYAML' instance.
--
-- @
--
-- instance 'ToYAML' Person where
--     \-- this generates a 'Node'
--     'toYAML' (Person n a) = 'mapping' [ "name" .= n, "age" .= a]
--
-- @
--
-- We can now 'encode' a node like so:
--
-- >>> encode [Person {name = "Vijay", age = 19}]
-- "age: 19\nname: Vijay\n"
--
-- There are predefined 'ToYAML' instances for many types. Here's an example encoding a complex Haskell Node'
--
-- >>> encode1 $ toYAML ([1,2,3], Map.fromList [(1, 2)])
-- "- - 1\n  - 2\n  - 3\n- 1: 2\n"
--


-- | A type from which YAML nodes can be constructed
--
-- @since 0.2.0.0
class ToYAML a where
  -- | Convert a Haskell Data-type to a YAML Node data type.
  toYAML :: a -> Node ()

instance Loc loc => ToYAML (Node loc) where
  toYAML = toUnit

instance ToYAML Bool where
  toYAML = Scalar () . SBool

instance ToYAML Double where
  toYAML = Scalar () . SFloat

instance ToYAML Int     where toYAML = Scalar () . SInt . toInteger
instance ToYAML Int8    where toYAML = Scalar () . SInt . toInteger
instance ToYAML Int16   where toYAML = Scalar () . SInt . toInteger
instance ToYAML Int32   where toYAML = Scalar () . SInt . toInteger
instance ToYAML Int64   where toYAML = Scalar () . SInt . toInteger
instance ToYAML Word    where toYAML = Scalar () . SInt . toInteger
instance ToYAML Word8   where toYAML = Scalar () . SInt . toInteger
instance ToYAML Word16  where toYAML = Scalar () . SInt . toInteger
instance ToYAML Word32  where toYAML = Scalar () . SInt . toInteger
instance ToYAML Word64  where toYAML = Scalar () . SInt . toInteger
instance ToYAML Natural where toYAML = Scalar () . SInt . toInteger
instance ToYAML Integer where toYAML = Scalar () . SInt


instance ToYAML Text where
  toYAML = Scalar () . SStr

-- | @since 0.2.1
instance ToYAML Scalar where
  toYAML = Scalar ()

instance ToYAML a => ToYAML (Maybe a) where
  toYAML Nothing  = Scalar () SNull
  toYAML (Just a) = toYAML a

-- instance (ToYAML a, ToYAML b) => ToYAML (Either a b) where
--     toYAML (Left a)  = toYAML a
--     toYAML (Right b) = toYAML b

instance ToYAML a => ToYAML [a] where
  toYAML = Sequence () tagSeq . map toYAML

instance (Ord k, ToYAML k, ToYAML v) => ToYAML (Map k v) where
  toYAML kv = Mapping () tagMap (Map.fromList $ map (\(k,v) -> (toYAML k , toYAML v)) (Map.toList kv))

instance (ToYAML a, ToYAML b) => ToYAML (a, b) where
  toYAML (a,b) = toYAML [toYAML a, toYAML b]

instance (ToYAML a, ToYAML b, ToYAML c) => ToYAML (a, b, c) where
  toYAML (a,b,c) = toYAML [toYAML a, toYAML b, toYAML c]

instance (ToYAML a, ToYAML b, ToYAML c, ToYAML d) => ToYAML (a, b, c, d) where
  toYAML (a,b,c,d) = toYAML [toYAML a, toYAML b, toYAML c, toYAML d]

instance (ToYAML a, ToYAML b, ToYAML c, ToYAML d, ToYAML e) => ToYAML (a, b, c, d, e) where
  toYAML (a,b,c,d,e) = toYAML [toYAML a, toYAML b, toYAML c, toYAML d, toYAML e]

instance (ToYAML a, ToYAML b, ToYAML c, ToYAML d, ToYAML e, ToYAML f) => ToYAML (a, b, c, d, e, f) where
  toYAML (a,b,c,d,e,f) = toYAML [toYAML a, toYAML b, toYAML c, toYAML d, toYAML e, toYAML f]

instance (ToYAML a, ToYAML b, ToYAML c, ToYAML d, ToYAML e, ToYAML f, ToYAML g) => ToYAML (a, b, c, d, e, f, g) where
  toYAML (a,b,c,d,e,f,g) = toYAML [toYAML a, toYAML b, toYAML c, toYAML d, toYAML e, toYAML f, toYAML g]


-- | Serialize YAML Node(s) using the YAML 1.2 Core schema to a lazy 'Data.YAML.Token.UTF8' encoded 'BS.L.ByteString'.
--
-- Each YAML Node produces exactly one YAML Document.
--
-- Here is an example of encoding a list of strings to produce a list of YAML Documents
--
-- >>> encode (["Document 1", "Document 2"] :: [Text])
-- "Document 1\n...\nDocument 2\n"
--
-- If we treat the above list of strings as a single sequence then we will produce a single YAML Document having a single sequence.
--
-- >>> encode ([["Document 1", "Document 2"]] :: [[Text]])
-- "- Document 1\n- Document 2\n"
--
-- Alternatively, if you only need a single YAML document in a YAML stream you might want to use the convenience function 'encode1'; or, if you need more control over the encoding, see 'encodeNode''.
--
-- @since 0.2.0
encode :: ToYAML v => [v] -> BS.L.ByteString
encode vList = encodeNode $ map (Doc . toYAML) vList

-- | Convenience wrapper over 'encode' taking exactly one YAML Node.
-- Hence it will always output exactly one YAML Document
--
-- Here is example of encoding a list of strings to produce exactly one of YAML Documents
--
-- >>> encode1 (["Document 1", "Document 2"] :: [Text])
-- "- Document 1\n- Document 2\n"
--
-- @since 0.2.0
encode1 :: ToYAML v => v -> BS.L.ByteString
encode1 a = encode [a]

-- | Like 'encode' but outputs 'BS.ByteString'
--
-- @since 0.2.0
encodeStrict :: ToYAML v => [v] -> BS.ByteString
encodeStrict = bsToStrict . encode

-- | Like 'encode1' but outputs a strict 'BS.ByteString'
--
-- @since 0.2.0
encode1Strict :: ToYAML v => v -> BS.ByteString
encode1Strict = bsToStrict . encode1

-- Internal helper
class Loc loc where
  toUnit :: Functor f => f loc -> f ()
  toUnit = (() <$)

instance Loc Pos

instance Loc () where toUnit = id

-- | Represents a key-value pair in YAML t'Mapping's
--
-- See also '.=' and 'mapping'
--
-- @since 0.2.1
type Pair = (Node (), Node ())

-- | @since 0.2.0
(.=) :: ToYAML a => Text -> a -> Pair
name .= node = (toYAML name, toYAML node)

-- | @since 0.2.0
mapping :: [Pair] -> Node ()
mapping = Mapping () tagMap . Map.fromList
