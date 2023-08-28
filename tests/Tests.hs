{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

import           Control.Monad
import           Control.Applicative
import           Data.Scientific
import           Data.YAML                  as Y
import qualified Data.Text                  as T
import qualified Data.Map                   as Map
import qualified Data.ByteString.Lazy.Char8 as BS.L
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty,Arbitrary(..))

outputStr :: ToYAML a => a -> BS.L.ByteString
outputStr a = BS.L.init (encode1 a)  -- TODO: remove trailing newline from Writer.hs

roundTripInt :: Int -> Bool
roundTripInt i = BS.L.pack (show i) == outputStr i

roundTripBool :: Bool -> Bool
roundTripBool b
  | b = "true"  == outputStr b
  | otherwise = "false" == outputStr b

roundTrip :: (Eq a, FromYAML a, ToYAML a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ v =
    case decode1 (encode1 v) :: (FromYAML a) => (Either (Pos, String) a) of
      Left _    -> False
      Right ans -> ans `eq` v

roundTripEq :: (Eq a, FromYAML a, ToYAML a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

main :: IO ()
main = defaultMain (testGroup "tests" tests)

tests :: [TestTree]
tests = 
  [ testGroup "encode" 
    [ testProperty "encodeInt" roundTripInt
    , testProperty "encodeBool" roundTripBool
    ]
  , testGroup "roundTrip" 
    [ testProperty "Bool"       $ roundTripEq True
    , testProperty "Int"        $ roundTripEq (1::Int)
    , testProperty "Integer"    $ roundTripEq (1::Integer)
    , testProperty "Scientific" $ roundTripEq (1::Scientific)
    , testProperty "Text"       $ roundTripEq T.empty
    , testProperty "Seq"        $ roundTripEq ([""]:: [T.Text])
    , testProperty "Map"        $ roundTripEq (undefined :: Map.Map T.Text T.Text)
    , testProperty "Foo"        $ roundTripEq (undefined :: Foo)
    ]
  ]

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

data Foo = Foo 
  { fooBool :: Bool
  , fooInt :: Int
  , fooTuple :: (T.Text, Int)
  , fooSeq :: [T.Text]
  , fooMap :: Map.Map T.Text T.Text
  } deriving (Show,Eq)

instance ToYAML Foo where
  toYAML Foo{..} = mapping [ "fooBool"  .= fooBool
                           , "fooInt"   .= fooInt
                           , "fooTuple" .= fooTuple
                           , "fooSeq"   .= fooSeq
                           , "fooMap"   .= fooMap
                           ]

instance FromYAML Foo where
  parseYAML = withMap "Foo" $ \m -> Foo
      <$> m .: "fooBool"
      <*> m .: "fooInt"
      <*> m .: "fooTuple"
      <*> m .: "fooSeq"
      <*> m .: "fooMap"

instance Arbitrary Foo where
  arbitrary = liftM5 Foo arbitrary arbitrary arbitrary arbitrary arbitrary
