{-# LANGUAGE OverloadedStrings   #-}

import Data.YAML as Y
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BS.L
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

outputStr :: ToYAML a => a -> BS.L.ByteString
outputStr a = BS.L.init (encode1 a)  -- TODO: remove trailing newline from Writer.hs

encodeInt :: Int -> Bool
encodeInt i = BS.L.pack (show i) == outputStr i

encodeBool :: Bool -> Bool
encodeBool b
  | b = "true"  == outputStr b
  | otherwise = "false" == outputStr b

encodeDouble :: Double -> Double -> Bool
encodeDouble num denom
    | d /= d      = ".nan"  == outputStr d
    | d == (1/0)  = ".inf"  == outputStr d
    | d == (-1/0) = "-.inf" == outputStr d
    | otherwise    = BS.L.pack (show d) == outputStr d
  where d = num / denom

roundTripStr :: String -> Bool
roundTripStr v = do
  let text = T.pack v
  case decode1 (encode1 text) of
    Left _  -> False
    Right ans -> text == ans

roundTrip :: (Eq a, FromYAML a, ToYAML a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ v =
    case decode1 (encode1 v) :: (FromYAML a) => (Either String a) of
      Left _    -> False
      Right ans -> ans `eq` v

approxEq :: Double -> Double -> Bool
approxEq a b = a == b || d < maxAbsoluteError || d / max (abs b) (abs a) <= maxRelativeError
    where 
      d = abs (a - b)
      maxAbsoluteError = 1e-15
      maxRelativeError = 1e-15

roundTripEq :: (Eq a, FromYAML a, ToYAML a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

main :: IO ()
main = defaultMain (testGroup "tests" tests)

tests :: [TestTree]
tests = 
  [ testGroup "encode" 
    [ testProperty "encodeInt" encodeInt
    , testProperty "encodeBool" encodeBool
    , testProperty "encodeDouble" encodeDouble
    ]
  , testGroup "roundTrip" 
    [ testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTrip approxEq (1::Double)
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "String" $ roundTripStr -- Writer fails for examples like "\\ "
    ]
  ]