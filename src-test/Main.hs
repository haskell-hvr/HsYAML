module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS.L
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Data.Text                  as T
import           Data.YAML
import           Data.YAML.Event            as YE
import qualified Data.YAML.Internal         as YI

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("yaml2event":args')
      | null args' -> cmdYaml2Event
      | otherwise -> do
          hPutStrLn stderr "unexpected arguments passed to yaml2event sub-command"
          exitFailure
    ("testmode":args') -> cmdTestmode args'
    _ -> do
      hPutStrLn stderr "usage: yaml-test <command> [<args>]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr ""
      hPutStrLn stderr "  yaml2event       reads YAML stream from STDIN and dumps events to STDOUT"
      hPutStrLn stderr "  testmode         internal test-mode"
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

cmdTestmode :: [FilePath] -> IO ()
cmdTestmode args = do
  forM_ args $ \d -> do
    let inYaml = d </> "in.yaml"
        testEv = d </> "test.event"

    exInput <- (&&) <$> doesFileExist inYaml <*> doesFileExist testEv

    when exInput $ do
      print inYaml
      inYamlDat <- BS.L.readFile inYaml
      testEvDat <- lines <$> readFile testEv

      isErr <- doesFileExist $ d </> "error"

      case sequence (parseEvents inYamlDat) of
        Left err
          | isErr -> do
              putStrLn "OK! (error)"
          | otherwise -> do
              print ("FAIL", err)
              putStrLn ""
              print testEvDat
              putStrLn ""
              BS.L.putStrLn inYamlDat
              putStrLn ""
              testParse inYamlDat
              putStrLn ""
              forM_ (parseEvents inYamlDat) print
              putStrLn ""
              putStrLn "----------------------------------------------------------------------------"
        Right evs' -> do
          let evs'' = map ev2str evs'
          if evs'' == testEvDat
             then putStrLn "OK!"
             else do
               putStrLn "FAIL!"
               when isErr $
                 putStrLn "(unexpected parser success)"
               print testEvDat
               print evs''
               putStrLn ""
               testParse inYamlDat
               putStrLn ""
               forM_ (parseEvents inYamlDat) print
               putStrLn ""
               putStrLn "----------------------------------------------------------------------------"
      putStrLn ""

      return ()

  return ()

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
testParse bs0 = mapM_  print $ YI.yaml "" bs0 False
