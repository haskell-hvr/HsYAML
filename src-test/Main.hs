{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-3.0
--
module Main where

import           Control.Monad
import           Control.Monad.Identity
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS.L
import           Data.Int                   (Int64)
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Read

import qualified Data.Aeson.Micro           as J
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T

import           Data.YAML                  as Y
import           Data.YAML.Event            as YE
import qualified Data.YAML.Token            as YT

import qualified TML

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("yaml2event":args')
      | null args' -> cmdYaml2Event
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to yaml2event sub-command"
          exitFailure

    ("yaml2json":args')
      | null args' -> cmdYaml2Json
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to yaml2json sub-command"
          exitFailure

    ("run-tml":args') -> cmdRunTml args'

    ("testml-compiler":args') -> cmdTestmlCompiler args'

    _ -> do
      hPutStrLn stderr "usage: yaml-test <command> [<args>]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr ""
      hPutStrLn stderr "  yaml2event       reads YAML stream from STDIN and dumps events to STDOUT"
      hPutStrLn stderr "  yaml2json        reads YAML stream from STDIN and dumps JSON to STDOUT"
      hPutStrLn stderr "  run-tml          run/validate YAML-specific .tml file(s)"
      hPutStrLn stderr "  testml-compiler  emulate testml-compiler"

      exitFailure


cmdYaml2Event :: IO ()
cmdYaml2Event = do
  inYamlDat <- BS.L.getContents
  forM_ (parseEvents inYamlDat) $ \ev -> case ev of
    Left (ofs,msg) -> do
      case msg of
        "" -> hPutStrLn stderr ("parsing error near byte offset " ++ show ofs)
        _  -> hPutStrLn stderr ("parsing error near byte offset " ++ show ofs ++ " (" ++ msg ++ ")")
      exitFailure
    Right event -> do
      hPutStrLn stdout (ev2str event)
      hFlush stdout


decodeAeson :: BS.L.ByteString -> Either String [J.Value]
decodeAeson bs0 = runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t sty v -> pure $ scalar2json t sty v
                            , ySequence = \t vs  -> pure $ Right (J.toJSON vs)
                            , yMapping  = \t kvs -> pure $ (J.object <$> go kvs)
                            , yAlias    = \_ c n -> pure $ if c then Left "cycle detected" else Right n
                            , yAnchor   = \_ n   -> pure $ Right n
                            }

    scalar2json :: Maybe T.Text -> YE.Style -> T.Text -> Either String J.Value
    scalar2json (Just t) _ v
      | t == "tag:yaml.org,2002:null" = Right J.Null
      | t == "tag:yaml.org,2002:str"  = Right (J.String v)
      | t == "tag:yaml.org,2002:int"  = maybe (Left "invalid int") (Right . J.Number) $ decodeNumber v
      | t == "tag:yaml.org,2002:float" = Left "invalid float"
      | t == "tag:yaml.org,2002:bool" = case v of
                                          "false" -> Right (J.Bool False)
                                          "true"  -> Right (J.Bool True)
                                          _       -> Left ("invalid bool")
      | t == "!" = Right (J.String v)
      | t == "?" = error (show t)
      | t == "" = error (show t)
    scalar2json Nothing YE.Plain v -- corresponds to '?' tag -- we apply core-schema rules
      | v == "true"  = Right (J.Bool True)
      | v == "false" = Right (J.Bool False)
      | v == "null"  = Right (J.Null)
      | v == ""      = Right (J.Null)
      | Just n <- decodeNumber v = Right (J.Number n)
      -- | otherwise    = Left ("couldn't resolve " ++ show v)
    scalar2json _ _ v = Right (J.String v)

    go :: [(J.Value,J.Value)] -> Either String [(T.Text, J.Value)]
    go = mapM g
      where
        -- for numbers and @null@ we apply some implicit conversions
        g :: (J.Value,J.Value) -> Either String (T.Text,J.Value)
        g (J.Number t,v) = case doubleToInt64 t of
                             Nothing -> Right (T.pack (show t),v)
                             Just i  -> Right (T.pack (show i),v)
        g (J.String t,v) = Right (t,v)
        g (J.Null,v)     = Right ("",v)
        g (k,_)          = Left ("dictionary entry had non-string key " ++ show k)



-- | Try to convert 'Double' into 'Int64', return 'Nothing' if not
-- representable loss-free as integral 'Int64' value.
doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64)
    = Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x


decodeNumber :: T.Text -> Maybe Double
decodeNumber = readMaybe . T.unpack -- fixme

cmdYaml2Json :: IO ()
cmdYaml2Json = do
  inYamlDat <- BS.L.getContents

  case decodeAeson inYamlDat of
    Left e -> fail e
    Right vs -> do
      forM_ vs $ \v -> BS.L.putStrLn (J.encode v)

  return ()

unescapeSpcTab :: T.Text -> T.Text
unescapeSpcTab = T.replace "<SPC>" " " . T.replace "<TAB>" "\t"

cmdRunTml :: [FilePath] -> IO ()
cmdRunTml args = do
  results <- forM args $ \fn -> do
    tml <- BS.readFile fn

    hPutStr stdout (fn ++ " : ")
    hFlush stdout

    TML.Document _ blocks <- either (fail . T.unpack) pure $ TML.parse fn (T.decodeUtf8 tml)

    forM blocks $ \(TML.Block label points) -> do

      let dats = [ (k,v) | TML.PointStr k v <- points ]

      let isErr = isJust (lookup "error" dats)

          Just inYamlDat = BS.L.fromStrict . T.encodeUtf8 . unescapeSpcTab <$> lookup "in-yaml" dats
          Just testEvDat = lines . T.unpack . unescapeSpcTab <$> lookup "test-event" dats

          mInJsonDat :: Maybe [J.Value]
          mInJsonDat = (maybe (error ("invalid JSON in " ++ show fn)) id . J.decodeStrictN . T.encodeUtf8) <$> lookup "in-json" dats

      case sequence (parseEvents inYamlDat) of
        Left err
          | isErr -> do
              putStrLn "OK! (error)"
              pure True
          | otherwise -> do
              putStrLn "FAIL!"
              putStrLn ""
              putStrLn "----------------------------------------------------------------------------"
              putStrLn' (T.unpack label)
              putStrLn ""
              putStrLn' (show err)
              putStrLn ""
              putStrLn' (show testEvDat)
              putStrLn ""
              BS.L.putStr inYamlDat
              putStrLn ""
              testParse inYamlDat
              putStrLn ""
              -- forM_ (parseEvents inYamlDat) (putStrLn' . show)
              putStrLn ""
              putStrLn "----------------------------------------------------------------------------"
              putStrLn ""
              pure False

        Right evs' -> do
          let evs'' = map ev2str evs'
          if evs'' == testEvDat
             then do

               case mInJsonDat of
                 Nothing -> do
                   putStrLn "OK!"
                   pure True
                 Just inJsonDat -> do
                   iutJson <- either fail pure $ decodeAeson inYamlDat

                   if iutJson == inJsonDat
                     then do
                       putStrLn "OK! (+JSON)"
                       pure True
                     else do
                       putStrLn "FAIL! (bad JSON)"

                       putStrLn' ("ref = " ++ show inJsonDat)
                       putStrLn' ("iut = " ++ show iutJson)

                       pure False

             else do
               if isErr
                 then putStrLn "FAIL! (unexpected parser success)"
                 else putStrLn "FAIL!"

               putStrLn ""
               putStrLn "----------------------------------------------------------------------------"
               putStrLn' (T.unpack label)
               putStrLn ""
               putStrLn' ("ref = " ++ show testEvDat)
               putStrLn' ("iut = " ++ show evs'')
               putStrLn ""
               BS.L.putStr inYamlDat
               putStrLn ""
               testParse inYamlDat
               putStrLn ""
               -- forM_ (parseEvents inYamlDat) (putStrLn' . show)
               putStrLn ""
               putStrLn "----------------------------------------------------------------------------"
               putStrLn ""
               pure False

  putStrLn ""

  let ok = length (filter id results')
      nok = length (filter not results')
      results' = concat results

  putStrLn ("done (passed: " ++ show ok ++ " / failed: " ++ show nok ++ ")")


-- | Incomplete proof-of-concept 'testml-compiler' operation
cmdTestmlCompiler :: [FilePath] -> IO ()
cmdTestmlCompiler [fn0] = do
  (fn,raw) <- case fn0 of
    "-" -> (,) "<stdin>" <$> T.getContents
    _   -> (,) fn0 <$> T.readFile fn0

  case TML.parse fn raw of
    Left e    -> T.hPutStrLn stderr e >> exitFailure
    Right doc -> BS.putStrLn (J.encodeStrict doc)
cmdTestmlCompiler _ = do
  hPutStrLn stderr ("Usage: yaml-test testml-compiler [ <testml-file-name> | - ]")
  exitFailure


putStrLn' :: String -> IO ()
putStrLn' msg = putStrLn ("  " ++ msg)


ev2str :: Event -> String
ev2str StreamStart           = "+STR"
ev2str (DocumentStart True)  = "+DOC ---"
ev2str (DocumentStart False) = "+DOC"
ev2str MappingEnd            = "-MAP"
ev2str (MappingStart manc mtag)    = "+MAP" ++ ancTagStr manc mtag
ev2str SequenceEnd           = "-SEQ"
ev2str (SequenceStart manc mtag)   = "+SEQ" ++ ancTagStr manc mtag
ev2str (DocumentEnd True) = "-DOC ..."
ev2str (DocumentEnd False) = "-DOC"
ev2str StreamEnd             = "-STR"
ev2str (Alias a)             = "=ALI *" ++ T.unpack a
ev2str (YE.Scalar manc mtag sty v) = "=VAL" ++ ancTagStr manc mtag ++ v'
  where
    v' = case sty of
           Plain        -> " :"  ++ quote2 v
           DoubleQuoted -> " \"" ++ quote2 v
           Literal      -> " |"  ++ quote2 v
           Folded       -> " >"  ++ quote2 v
           SingleQuoted -> " '"  ++ quote2 v

ancTagStr manc mtag = anc' ++ tag'
  where
    anc' = case manc of
             Nothing  -> ""
             Just anc -> " &" ++ T.unpack anc

    tag' = case mtag of
             Nothing -> ""
             Just t  -> " <" ++ T.unpack t ++ ">"


quote2 :: T.Text -> String
quote2 = concatMap go . T.unpack
  where
    go c | c == '\n' = "\\n"
         | c == '\t' = "\\t"
         | c == '\b' = "\\b"
         | c == '\r' = "\\r"
         | c == '\\' = "\\\\"
         | otherwise = [c]



testParse :: BS.L.ByteString -> IO ()
testParse bs0 = mapM_  (putStrLn' . showT) $ YT.tokenize bs0 False
  where
    showT :: YT.Token -> String
    showT t = replicate (YT.tLineChar t) ' ' ++ show (YT.tText t) ++ "  " ++ show (YT.tCode t)
