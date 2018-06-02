-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-3.0
--
module Main where

import           Control.Monad
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BS.L
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.YAML
import           Data.YAML.Event            as YE
import qualified Data.YAML.Token            as YI

import           TML

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("yaml2event":args')
      | null args' -> cmdYaml2Event
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to yaml2event sub-command"
          exitFailure
    ("run-tml":args') -> cmdRunTml args'
    _ -> do
      hPutStrLn stderr "usage: yaml-test <command> [<args>]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr ""
      hPutStrLn stderr "  yaml2event       reads YAML stream from STDIN and dumps events to STDOUT"
      hPutStrLn stderr "  run-tml          run/validate .tml file(s)"
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


cmdRunTml :: [FilePath] -> IO ()
cmdRunTml args = do
  results <- forM args $ \fn -> do
    tml <- BS.readFile fn

    hPutStr stdout (fn ++ " : ")
    hFlush stdout

    (label, dats) <- maybe (fail ("failed to parse " ++ fn)) pure $ decodeTml tml

    let isErr = isJust (lookup "error" dats)

        Just inYamlDat = BS.L.fromStrict   <$> lookup "in.yaml" dats
        Just testEvDat = lines . T.unpack . T.decodeUtf8 <$> lookup "test.event" dats

    case sequence (parseEvents inYamlDat) of
      Left err
        | isErr -> do
            putStrLn "OK! (error)"
            pure True
        | otherwise -> do
            putStrLn "FAIL!"
            putStrLn ""
            putStrLn "----------------------------------------------------------------------------"
            putStrLn' (BS.unpack label)
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
             putStrLn "OK!"
             pure True

           else do
             if isErr
               then putStrLn "FAIL! (unexpected parser success)"
               else putStrLn "FAIL!"

             putStrLn ""
             putStrLn "----------------------------------------------------------------------------"
             putStrLn' (BS.unpack label)
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

  let ok = length (filter id results)
      nok = length (filter not results)

  putStrLn ("done (passed: " ++ show ok ++ " / failed: " ++ show nok ++ ")")

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
testParse bs0 = mapM_  (putStrLn' . showT) $ YI.yaml "" bs0 False
  where
    showT :: YI.Token -> String
    showT t = replicate (YI.tLineChar t) ' ' ++ show (YI.tText t) ++ "  " ++ show (YI.tCode t)
