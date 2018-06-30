{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Incomplete TestML 0.3.0 parser
module TML
    ( TML.parse

    , Document(..)
    , Block(..)

    , Point(..)
    , PseudoId(..)

    , Code(..)
    , AssertOp(..)
    , CodeExpr(..)
    , CodeObject(..)
    , FunCall(..)
    ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B

import           Control.Applicative        hiding (many, some)
import           Control.Monad
import qualified Data.Aeson.Micro           as J
import qualified Data.ByteString            as BS
import qualified Data.Char                  as C
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import           Data.Void
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

type Parser = Parsec Void T.Text

parse :: String -> T.Text -> Either T.Text Document
parse fn raw = either (Left . T.pack . parseErrorPretty' raw)
                      (Right .process_pseudo)
                      (Text.Megaparsec.parse testml_document fn raw)


----------------------------------------------------------------------------

data Document = Document [Code] [Block]
              deriving Show

instance J.ToJSON Document where
  toJSON (Document code dat)
    = J.object [ "testml" J..= ("0.3.0" :: T.Text)
               , "code" J..= code
               , "data" J..= dat
               ]

data Block = Block !T.Text [Point]
           deriving Show

instance J.ToJSON Block where
  toJSON (Block label points)
    = J.object [ "label" J..= label
               , "point" J..= J.object (map f points)
               ]
    where
      f (PointStr k v)  = k J..= v
      f (PointPseudo k) = (T.pack (show k)) J..= True
      f (PointInt k v)  = k J..= v

data Point = PointStr !T.Text !T.Text
           | PointInt !T.Text !Integer
           | PointPseudo !PseudoId
           deriving Show

instance J.ToJSON Code where
  toJSON (CodeAssignmentStmt lhs rhs)
    = J.Array [J.String "=", J.String lhs, J.toJSON rhs]
  toJSON stmt@(CodeExpressionStmt lhs massert)
    | pobjs@(_:_) <- pointObjsInExpr stmt
      = J.Array [ J.String "%()"
                , J.Array [ J.String ("*" `mappend` p) | p <- pobjs ]
                , expr'
                ]
    | otherwise = expr'
    where
      expr' = case massert of
                Just (op,rhs) -> J.toJSON (op,lhs,rhs)
                Nothing       -> J.toJSON lhs

data Code = CodeAssignmentStmt !T.Text !CodeExpr
          | CodeExpressionStmt !CodeExpr !(Maybe (AssertOp,CodeExpr))
          | CodeImportStmt [T.Text]
          deriving Show

instance J.ToJSON AssertOp where
  toJSON AssertEq   = J.String "=="
  toJSON AssertHas  = J.String "~~"
  toJSON AssertLike = J.String "=~"

data AssertOp = AssertEq | AssertHas | AssertLike
              deriving Show

instance J.ToJSON CodeExpr where
  toJSON (CodeExpr obj [])  = J.toJSON obj
  toJSON (CodeExpr obj fns) = J.Array $ [J.String ".", J.toJSON obj] ++ map J.toJSON fns

data CodeExpr = CodeExpr !CodeObject [FunCall]
  deriving Show

instance J.ToJSON CodeObject where
  toJSON (StrObj s)   = J.String s
  toJSON (NumObj n)   = J.Number n
  toJSON (PointObj j) = J.Array [J.String "*", J.String j]
  toJSON (CallObj fn) = J.toJSON fn

data CodeObject = StrObj !T.Text
                | CallObj !FunCall
                | NumObj !Double
                | PointObj !T.Text
                deriving Show

instance J.ToJSON FunCall where
  toJSON (FunCall fn args) = J.Array (J.String fn : map J.toJSON args)

data FunCall = FunCall !T.Text [CodeExpr]
  deriving Show



pointObjsInExpr :: Code -> [T.Text]
pointObjsInExpr co = nub $ case co of
    CodeAssignmentStmt _ expr           -> goExpr expr
    CodeExpressionStmt e1 Nothing       -> goExpr e1
    CodeExpressionStmt e1 (Just (_,e2)) -> goExpr e1 ++ goExpr e2
  where
    goExpr (CodeExpr obj fns) = goObj obj ++ concatMap goFun fns

    goFun (FunCall _ exprs) = concatMap goExpr exprs

    goObj (PointObj j) = [j]
    goObj (CallObj fn) = goFun fn
    goObj (StrObj _)   = []
    goObj (NumObj _)   = []


testml_document :: Parser Document
testml_document = Document <$> code_section <*> data_section <* eof

pseudo_point_name  :: Parser PseudoId
pseudo_point_name
  = choice [ HEAD <$ string "HEAD"
           , LAST <$ string "LAST"
           , ONLY <$ string "ONLY"
           , SKIP <$ string "SKIP"
           , TODO <$ string "TODO"
           , DIFF <$ string "DIFF"
           ]

data PseudoId = HEAD
              | LAST
              | ONLY
              | SKIP
              | TODO
              | DIFF
              deriving (Eq,Show)

process_pseudo :: Document -> Document
process_pseudo (Document code bs0) = Document code (go bs0)
  where
    go blocks
      | Just b <- find isOnly blocks' = [b]
      | Just bs <- goHead blocks'     = bs
      | Just bs <- goLast [] blocks'  = bs
      | otherwise = blocks'
      where
        blocks' = filter (not . isSkip) blocks

        isOnly b = ONLY `elem` pseudos b
        isSkip b = SKIP `elem` pseudos b
        isHead b = HEAD `elem` pseudos b
        isLast b = LAST `elem` pseudos b

        pseudos (Block _ ps) = [ k | PointPseudo k <- ps ]

        goHead [] = Nothing
        goHead (b:bs)
          | isHead b = Just (b:bs)
          | otherwise = goHead bs

        goLast acc [] = Nothing
        goLast acc (b:bs)
          | isLast b  = Just $ reverse (b:bs)
          | otherwise = goLast (b:acc) bs

code_section :: Parser [Code]
code_section = do
    xs <- many code_statement
    pure (catMaybes xs)
  where
    code_statement = choice
      [ Nothing <$ comment_lines
      , Just <$> import_directive
      , Just <$> assignment_statement
      , Just <$> expression_statement
      ]

    import_directive = do
      string "%Import"
      ws
      mods <- module_name `sepBy1` ws
      ws0
      eol
      pure $! CodeImportStmt mods

    module_name :: Parser T.Text
    module_name = T.pack <$> some alphaNumChar

    assignment_statement = do
      v <- try $ do
        v' <- identifier_name
        ws
        void (char '=') <|> void (string "||=") -- FIXME
        ws
        pure v'
      e <- code_expression
      eol
      pure (CodeAssignmentStmt v e)

    expression_statement = do
      -- TODO: expression-label
      -- optional (double_string >> char ':' >> ws0)

      -- TODO: pick-expression

      lhs <- code_expression
      ws
      op <- choice
            [ AssertEq   <$ string "=="
            , AssertHas  <$ string "~~"
            , AssertLike <$ string "=~"
            ]
      ws
      rhs <- code_expression

      optional $ do
        ws0
        char ':'
        double_string

      eol

      pure (CodeExpressionStmt lhs (Just (op,rhs)))


code_expression :: Parser CodeExpr
code_expression = CodeExpr <$> code_object <*> many function_call


-- quoted string
double_string :: Parser T.Text
double_string = do
    char '"'
    str <- many (noneOf ("\n\"\\" :: [Char]) <|> (char '\\' >> (unesc <$> oneOf ("\\\"0nt" :: [Char]))))
    char '"'
    pure $! (T.pack str)
  where
    unesc '0' = '\0'
    unesc 'n' = '\n'
    unesc 't' = '\t'
    unesc c   = c

single_string :: Parser T.Text
single_string = do
    char '\''
    str <- many (noneOf ("\n'\\" :: [Char]) <|> (char '\\' >> (oneOf ("\\'" :: [Char]))))
    char '\''
    pure $! (T.pack str)

function_call :: Parser FunCall
function_call = do
  char '.'
  call_object

call_object :: Parser FunCall
call_object = FunCall <$> identifier_name
                      <*> optional' [] (between (char '(') (char ')') $ code_expression `sepBy1` (char ',' >> ws0))

optional' def p = do
  x <- optional p
  case x of
    Nothing -> pure def
    Just y  -> pure y

code_object :: Parser CodeObject
code_object
  = choice
    [ mkPoint <$> char '*' <*> lowerChar <*> many (lowerChar <|> digitChar <|> char '-' <|> char '_')
    , mkNum <$> optional (char '-') <*> some digitChar <*> optional (char '.' >> some digitChar)
    , CallObj <$> call_object
    , StrObj <$> single_string
    , StrObj <$> double_string
    ] <?> "code-object"
  where
    mkPoint _ c cs = PointObj $! (T.pack (c:cs))
    mkNum msign ds1 mds2 = NumObj $! (read $ (maybe id (:) msign) ds1 ++ (maybe "" ('.':) mds2))

data_section :: Parser [Block]
data_section = many block_definition
  where
    block_definition = do
      -- block_heading
      string "===" *> ws
      l <- T.pack <$> manyTill anyChar eol

      -- TODO: user_defined
      ps <- many point_definition
      pure (Block l ps)

    point_definition = do
      string "---" *> ws

      j <- eitherP identifier_user pseudo_point_name

      filters <- maybe [] id <$> optional filter_spec

      let single = do
            _ <- char ':' *> ws
            x <- T.pack <$> manyTill anyChar eol
            -- consume and ignore any point_lines
            _ <- point_lines
            pure $! case j of
                      Left j'  -> mkSinglePointVal j' (transformPoint True filters x)
                      Right j' -> PointPseudo j' -- is this allowed?

          multi = do
            ws0 *> eol
            x <- point_lines
            pure $! case j of
                      Left j'  -> PointStr j' (transformPoint False filters x)
                      Right j' -> PointPseudo j'

      single <|> multi

    filter_spec = between (char '(') (char ')') $ many (oneOf ("<#+-~/@" :: [Char]))

    mkSinglePointVal k v
      | T.all C.isDigit v = PointInt k (read (T.unpack v))
      | otherwise         = PointStr k v

point_lines :: Parser T.Text
point_lines = T.pack . unlines <$> go
  where
    go = many (notFollowedBy point_boundary *> manyTill anyChar eol)

    point_boundary :: Parser ()
    point_boundary = void (string "---") <|> void (string "===") <|> eof

identifier_user :: Parser T.Text
identifier_user = do
  x  <- (:) <$> lowerChar <*> many alphaNumChar
  xs <- many ((:) <$> char '-' <*> some alphaNumChar)

  pure $! T.pack (concat (x:xs))

identifier_name :: Parser T.Text
identifier_name = do
  x  <- (:) <$> letterChar <*> many alphaNumChar
  xs <- many ((:) <$> char '-' <*> some alphaNumChar)

  pure $! T.pack (concat (x:xs))


ws :: Parser ()
ws = void $ takeWhile1P (Just "BLANK") (\c -> c == ' ' || c == '\t')

ws0 :: Parser ()
ws0 = void $ takeWhileP (Just "BLANK") (\c -> c == ' ' || c == '\t')

blank_line :: Parser ()
blank_line = (try (ws0 <* eol) <|> try (ws <* eof)) <?> "blank-line"

comment_line :: Parser ()
comment_line = (char '#' *> takeWhileP Nothing (/= '\n') *> void eol) <?> "comment-line"

comment_lines :: Parser ()
comment_lines = void (some (comment_line <|> blank_line))

stripTrailEols :: T.Text -> T.Text
stripTrailEols = go
  where
    go t | T.isSuffixOf "\n\n" t     = go (T.init t)
         | T.isSuffixOf "\r\n\r\n" t = go (T.init (T.init t))
         | t == "\n"                 = ""
         | otherwise = t

-- 'undent'
stripPrefixInd :: T.Text -> T.Text
stripPrefixInd = T.unlines . map go . T.lines
  where
    go t | T.isPrefixOf "    " t = T.drop 4 t
         | T.isPrefixOf "   "  t = T.drop 3 t
         | T.isPrefixOf "  "   t = T.drop 2 t
         | T.isPrefixOf " "    t = T.drop 1 t
         | otherwise           = t

stripComments :: T.Text -> T.Text
stripComments = T.unlines . filter (not . T.isPrefixOf "#") . T.lines

transformPoint :: Bool -> [Char] -> T.Text -> T.Text
transformPoint single mods0 -- TODO: backslash
  = go mods0 .
    (if keepBlanks then id else stripTrailEols) .
    (if keepComments then id else stripComments)
  where
    keepBlanks = single || ('+' `elem` mods0)
    keepComments = single || ('#' `elem` mods0)

    go []       = id
    go ('<':xs)
      | single = error "invalid filter for point-single"
      | otherwise = go xs . stripPrefixInd
    go ('+':xs) = go xs -- negative flag
    go ('#':xs) = go xs -- negative flag
    go ('-':xs) = go xs . T.dropWhileEnd C.isSpace
    go (c:_)    = error ("unknown filter " ++ show c)
