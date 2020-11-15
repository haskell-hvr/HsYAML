{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PostfixOperators       #-}
{-# LANGUAGE Safe                   #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Copyright: © Oren Ben-Kiki 2007,
--            © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Tokenizer for the YAML 1.2 syntax as defined in <http://yaml.org/spec/1.2/spec.html>.
--
module Data.YAML.Token
  ( tokenize
  , Token(..)
  , Code(..)
  , Encoding(..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.DList                 as D
import           Prelude                    hiding ((*), (+), (-), (/), (^))
import qualified Prelude

import           Data.YAML.Token.Encoding   (Encoding (..), decode)

import           Util                       hiding (empty)
import qualified Util

-- * Generic operators
--
-- ** Numeric operators
--
-- We rename the four numerical operators @+@ @-@ @*@ @\/@ to start with @.@
-- (@.+@, @.-@, @.*@, @.\/@). This allows us to use the originals for BNF
-- notation (we also hijack the @^@ operator). This is not a generally
-- recommended practice. It is justified in this case since we have very little
-- arithmetic operations, and a lot of BNF rules which this makes extremely
-- readable.

infixl 6 .+
-- | \".+\" is the numeric addition (we use \"+\" for postfix \"one or more\").
(.+) :: Int -> Int -> Int
(.+) = (Prelude.+)

infixl 6 .-
-- | \".-\" is the numeric subtraction (we use \"-\" for infix \"and not\").
(.-) :: Int -> Int -> Int
(.-) = (Prelude.-)

{-
infixl 7 .*
-- | \".*\" is the numeric multiplication (we use \"*\" for postfix \"zero or
-- more\").
(.*) :: Int -> Int -> Int
(.*) = (Prelude.*)
-}

-- ** Record field access
--
-- We also define @^.@ for record access for increased readability.

infixl 8 ^.
-- | @record ^. field@ is the same as @field record@,  but is more readable.
--
-- NB: This trivially emulates the @lens@ operator
(^.) :: record -> (record -> value) -> value
record ^. field = field record

-- * Result tokens
--
-- The parsing result is a stream of tokens rather than a parse tree. The idea
-- is to convert the YAML input into \"byte codes\". These byte codes are
-- intended to be written into a byte codes file (or more likely a UNIX pipe)
-- for further processing.

-- | 'Token' codes.
data Code = Bom             -- ^ BOM, contains \"@TF8@\", \"@TF16LE@\", \"@TF32BE@\", etc.
          | Text            -- ^ Content text characters.
          | Meta            -- ^ Non-content (meta) text characters.
          | Break           -- ^ Separation line break.
          | LineFeed        -- ^ Line break normalized to content line feed.
          | LineFold        -- ^ Line break folded to content space.
          | Indicator       -- ^ Character indicating structure.
          | White           -- ^ Separation white space.
          | Indent          -- ^ Indentation spaces.
          | DirectivesEnd   -- ^ Document start marker.
          | DocumentEnd     -- ^ Document end marker.
          | BeginEscape     -- ^ Begins escape sequence.
          | EndEscape       -- ^ Ends escape sequence.
          | BeginComment    -- ^ Begins comment.
          | EndComment      -- ^ Ends comment.
          | BeginDirective  -- ^ Begins directive.
          | EndDirective    -- ^ Ends directive.
          | BeginTag        -- ^ Begins tag.
          | EndTag          -- ^ Ends tag.
          | BeginHandle     -- ^ Begins tag handle.
          | EndHandle       -- ^ Ends tag handle.
          | BeginAnchor     -- ^ Begins anchor.
          | EndAnchor       -- ^ Ends anchor.
          | BeginProperties -- ^ Begins node properties.
          | EndProperties   -- ^ Ends node properties.
          | BeginAlias      -- ^ Begins alias.
          | EndAlias        -- ^ Ends alias.
          | BeginScalar     -- ^ Begins scalar content.
          | EndScalar       -- ^ Ends scalar content.
          | BeginSequence   -- ^ Begins sequence content.
          | EndSequence     -- ^ Ends sequence content.
          | BeginMapping    -- ^ Begins mapping content.
          | EndMapping      -- ^ Ends mapping content.
          | BeginPair       -- ^ Begins mapping key:value pair.
          | EndPair         -- ^ Ends mapping key:value pair.
          | BeginNode       -- ^ Begins complete node.
          | EndNode         -- ^ Ends complete node.
          | BeginDocument   -- ^ Begins document.
          | EndDocument     -- ^ Ends document.
          | BeginStream     -- ^ Begins YAML stream.
          | EndStream       -- ^ Ends YAML stream.
          | Error           -- ^ Parsing error at this point.
          | Unparsed        -- ^ Unparsed due to errors (or at end of test).
          | Detected        -- ^ Detected parameter (for testing).
  deriving (Show,Eq,Generic)

-- | @since 0.2.0
instance NFData Code where
  rnf x = seq x ()

{-
-- | @show code@ converts a 'Code' to the one-character YEAST token code char.
-- The list of byte codes is also documented in the @yaml2yeast@ program.
instance Show Code where
  show code = case code of
                   Bom             -> "U"
                   Text            -> "T"
                   Meta            -> "t"
                   Break           -> "b"
                   LineFeed        -> "L"
                   LineFold        -> "l"
                   Indicator       -> "I"
                   White           -> "w"
                   Indent          -> "i"
                   DirectivesEnd   -> "K"
                   DocumentEnd     -> "k"
                   BeginEscape     -> "E"
                   EndEscape       -> "e"
                   BeginComment    -> "C"
                   EndComment      -> "c"
                   BeginDirective  -> "D"
                   EndDirective    -> "d"
                   BeginTag        -> "G"
                   EndTag          -> "g"
                   BeginHandle     -> "H"
                   EndHandle       -> "h"
                   BeginAnchor     -> "A"
                   EndAnchor       -> "a"
                   BeginProperties -> "P"
                   EndProperties   -> "p"
                   BeginAlias      -> "R"
                   EndAlias        -> "r"
                   BeginScalar     -> "S"
                   EndScalar       -> "s"
                   BeginSequence   -> "Q"
                   EndSequence     -> "q"
                   BeginMapping    -> "M"
                   EndMapping      -> "m"
                   BeginNode       -> "N"
                   EndNode         -> "n"
                   BeginPair       -> "X"
                   EndPair         -> "x"
                   BeginDocument   -> "O"
                   EndDocument     -> "o"
                   Error           -> "!"
                   Unparsed        -> "-"
                   Detected        -> "$"
-}

-- | Parsed token.
data Token = Token {
    tByteOffset :: !Int,   -- ^ 0-base byte offset in stream.
    tCharOffset :: !Int,   -- ^ 0-base character offset in stream.
    tLine       :: !Int,   -- ^ 1-based line number.
    tLineChar   :: !Int,   -- ^ 0-based character in line.
    tCode       :: !Code,  -- ^ Specific token 'Code'.
    tText       :: !String -- ^ Contained input chars, if any.
  } deriving (Show,Generic)

-- | @since 0.2.0
instance NFData Token where
  rnf Token { tText = txt } = rnf txt

-- * Parsing framework
--
-- Haskell has no shortage of parsing frameworks. We use our own because:
--
--  * Most available frameworks are inappropriate because of their focus on
--    building a parse tree, and completing all of it before any of it is
--    accessible to the caller. We return a stream of tokens, and would like
--    its head to be accessible as soon as possible to allow for streaming. To
--    do this with bounded memory usage we use a combination of continuation
--    passing style and difference lists for the collected tokens.
--
--  * Haskell makes it so easy to roll your own parsing framework. We need some
--    specialized machinery (limited lookahead, forbidden patterns). It is
--    possible to build these on top of existing frameworks but the end result
--    isn't much shorter than rolling our own.
--
-- Since we roll our own framework we don't bother with making it generalized,
-- so we maintain a single 'State' type rather than having a generic one that
-- contains a polymorphic \"UserState\" field etc.

-- | A 'Data.YAML.Token.Parser' is basically a function computing a 'Reply'.
newtype Parser result = Parser (State -> Reply result)

applyParser :: Parser result -> State -> Reply result
applyParser (Parser p) s = p s

-- | The 'Result' of each invocation is either an error, the actual result, or
-- a continuation for computing the actual result.
data Result result = Failed String        -- ^ Parsing aborted with a failure.
                   | Result result        -- ^ Parsing completed with a result.
                   | More (Parser result) -- ^ Parsing is ongoing with a continuation.

{-
-- Showing a 'Result' is only used in debugging.
instance (Show result) => Show (Result result) where
  show result = case result of
                     Failed message -> "Failed " ++ message
                     Result result  -> "Result " ++ (show result)
                     More _         -> "More"
-}

-- | Each invocation of a 'Data.YAML.Token.Parser' yields a 'Reply'. The 'Result' is only one
-- part of the 'Reply'.
data Reply result = Reply {
    rResult :: !(Result result), -- ^ Parsing result.
    rTokens :: !(D.DList Token), -- ^ Tokens generated by the parser.
    rCommit :: !(Maybe Decision),  -- ^ Commitment to a decision point.
    rState  :: !State            -- ^ The updated parser state.
  }

{-
-- Showing a 'State' is only used in debugging.
instance (Show result) => Show (Reply result) where
  show reply = "Result: "    ++ (show $ reply^.rResult)
            ++ ", Tokens: "  ++ (show $ D.toList $ reply^.rTokens)
            ++ ", Commit: "  ++ (show $ reply^.rCommit)
            ++ ", State: { " ++ (show $ reply^.rState) ++ "}"
-}

-- A 'Pattern' is a parser that doesn't have an (interesting) result.
type Pattern = Parser ()

-- ** Parsing state

-- | The internal parser state. We don't bother with parameterising it with a
-- \"UserState\", we just bundle the generic and specific fields together (not
-- that it is that easy to draw the line - is @sLine@ generic or specific?).
data State = State {
    sEncoding        :: !Encoding,        -- ^ The input UTF encoding.
    sDecision        :: !Decision,        -- ^ Current decision name.
    sLimit           :: !Int,             -- ^ Lookahead characters limit.
    sForbidden       :: !(Maybe Pattern), -- ^ Pattern we must not enter into.
    sIsPeek          :: !Bool,            -- ^ Disables token generation.
    sIsSol           :: !Bool,            -- ^ Is at start of line?
    sChars           :: ![Char],          -- ^ (Reversed) characters collected for a token.
    sCharsByteOffset :: !Int,             -- ^ Byte offset of first collected character.
    sCharsCharOffset :: !Int,             -- ^ Char offset of first collected character.
    sCharsLine       :: !Int,             -- ^ Line of first collected character.
    sCharsLineChar   :: !Int,             -- ^ Character in line of first collected character.
    sByteOffset      :: !Int,             -- ^ Offset in bytes in the input.
    sCharOffset      :: !Int,             -- ^ Offset in characters in the input.
    sLine            :: !Int,             -- ^ Builds on YAML's line break definition.
    sLineChar        :: !Int,             -- ^ Character number in line.
    sCode            :: !Code,            -- ^ Of token we are collecting chars for.
    sLast            :: !Char,            -- ^ Last matched character.
    sInput           :: ![(Int, Char)]    -- ^ The decoded input characters.
  }

{-
-- Showing a 'State' is only used in debugging. Note that forcing dump of
-- @sInput@ will disable streaming it.
instance Show State where
  show state = "Encoding: "          ++ (show $ state^.sEncoding)
            ++ ", Decision: "        ++ (show $ state^.sDecision)
            ++ ", Limit: "           ++ (show $ state^.sLimit)
            ++ ", IsPeek: "          ++ (show $ state^.sIsPeek)
            ++ ", IsSol: "           ++ (show $ state^.sIsSol)
            ++ ", Chars: >>>"        ++ (reverse $ state^.sChars) ++ "<<<"
            ++ ", CharsByteOffset: " ++ (show $ state^.sCharsByteOffset)
            ++ ", CharsCharOffset: " ++ (show $ state^.sCharsCharOffset)
            ++ ", CharsLine: "       ++ (show $ state^.sCharsLine)
            ++ ", CharsLineChar: "   ++ (show $ state^.sCharsLineChar)
            ++ ", ByteOffset: "      ++ (show $ state^.sByteOffset)
            ++ ", CharOffset: "      ++ (show $ state^.sCharOffset)
            ++ ", Line: "            ++ (show $ state^.sLine)
            ++ ", LineChar: "        ++ (show $ state^.sLineChar)
            ++ ", Code: "            ++ (show $ state^.sCode)
            ++ ", Last: "            ++ (show $ state^.sLast)
--          ++ ", Input: >>>"        ++ (show $ state^.sInput) ++ "<<<"
-}

-- | @initialState name input@ returns an initial 'State' for parsing the
-- /input/ (with /name/ for error messages).
initialState :: BLC.ByteString -> State
initialState input
  = State { sEncoding        = encoding
          , sDecision        = DeNone
          , sLimit           = -1
          , sForbidden       = Nothing
          , sIsPeek          = False
          , sIsSol           = True
          , sChars           = []
          , sCharsByteOffset = -1
          , sCharsCharOffset = -1
          , sCharsLine       = -1
          , sCharsLineChar   = -1
          , sByteOffset      = 0
          , sCharOffset      = 0
          , sLine            = 1
          , sLineChar        = 0
          , sCode            = Unparsed
          , sLast            = ' '
          , sInput           = decoded
          }
  where
    (encoding, decoded) = decode input

-- *** Setters
--
-- We need four setter functions to pass them around as arguments. For some
-- reason, Haskell only generates getter functions.

-- | @setLimit limit state@ sets the @sLimit@ field to /limit/.
setLimit :: Int -> State -> State
setLimit limit state = state { sLimit = limit }
{-# INLINE setLimit #-}

-- | @setForbidden forbidden state@ sets the @sForbidden@ field to /forbidden/.
setForbidden :: Maybe Pattern -> State -> State
setForbidden forbidden state = state { sForbidden = forbidden }
{-# INLINE setForbidden #-}

-- | @setCode code state@ sets the @sCode@ field to /code/.
setCode :: Code -> State -> State
setCode code state = state { sCode = code }
{-# INLINE setCode #-}

-- ** Implicit parsers
--
-- It is tedious to have to wrap each expected character (or character range)
-- in an explicit 'Parse' constructor. We let Haskell do that for us using a
-- 'Match' class.

-- | @Match parameter result@ specifies that we can convert the /parameter/ to
-- a 'Data.YAML.Token.Parser' returning the /result/.
class Match parameter result | parameter -> result where
    match :: parameter -> Parser result

-- | We don't need to convert a 'Data.YAML.Token.Parser', it already is one.
instance Match (Parser result) result where
    match = id

-- | We convert 'Char' to a parser for a character (that returns nothing).
instance Match Char () where
    match code = nextIf (== code)

-- | We convert a 'Char' tuple to a parser for a character range (that returns
-- nothing).
instance Match (Char, Char) () where
    match (low, high) = nextIf $ \ code -> low <= code && code <= high

-- | We convert 'String' to a parser for a sequence of characters (that returns
-- nothing).
instance Match String () where
    match = foldr (&) empty

-- ** Reply constructors

-- | @returnReply state result@ prepares a 'Reply' with the specified /state/
-- and /result/.
returnReply :: State -> result -> Reply result
returnReply state result = Reply { rResult = Result result,
                                   rTokens = D.empty,
                                   rCommit = Nothing,
                                   rState  = state }

-- | @tokenReply state token@ returns a 'Reply' containing the /state/ and
-- /token/. Any collected characters are cleared (either there are none, or we
-- put them in this token, or we don't want them).
tokenReply :: State -> Token -> Reply ()
tokenReply state token = Reply { rResult = Result (),
                                 rTokens = D.singleton token,
                                 rCommit = Nothing,
                                 rState  = state { sCharsByteOffset = -1,
                                                   sCharsCharOffset = -1,
                                                   sCharsLine       = -1,
                                                   sCharsLineChar   = -1,
                                                   sChars           = [] } }

-- | @failReply state message@ prepares a 'Reply' with the specified /state/
-- and error /message/.
failReply :: State -> String -> Reply result
failReply state message = Reply { rResult = Failed message,
                                  rTokens = D.empty,
                                  rCommit = Nothing,
                                  rState  = state }

-- | @unexpectedReply state@ returns a @failReply@ for an unexpected character.
unexpectedReply :: State -> Reply result
unexpectedReply state = case state^.sInput of
                             ((_, char):_) -> failReply state $ "Unexpected '" ++ [char] ++ "'"
                             []            -> failReply state "Unexpected end of input"


instance Functor Parser where
  fmap g f = Parser $ \state ->
    let reply = applyParser f state
    in case reply^.rResult of
       Failed message -> reply { rResult = Failed message }
       Result x       -> reply { rResult = Result (g x) }
       More parser    -> reply { rResult = More $ fmap g parser }


instance Applicative Parser where
  pure result = Parser $ \state -> returnReply state result

  (<*>) = ap

  left *> right = Parser $ \state ->
    let reply = applyParser left state
    in case reply^.rResult of
       Failed message -> reply { rResult = Failed message }
       Result _       -> reply { rResult = More right }
       More parser    -> reply { rResult = More $ parser *> right }

-- | Allow using the @do@ notation for our parsers, which makes for short and
-- sweet @do@ syntax when we want to examine the results (we typically don't).
instance Monad Parser where

  -- @return result@ does just that - return a /result/.
  return = pure

  -- @left >>= right@ applies the /left/ parser, and if it didn't fail
  -- applies the /right/ one (well, the one /right/ returns).
  left >>= right = Parser $ \state ->
    let reply = applyParser left state
    in case reply^.rResult of
       Failed message -> reply { rResult = Failed message }
       Result value   -> reply { rResult = More $ right value }
       More parser    -> reply { rResult = More $ parser >>= right }

  (>>) = (*>)

-- | @fail message@ does just that - fails with a /message/.
pfail :: String -> Parser a
pfail message = Parser $ \state -> failReply state message

-- ** Parsing operators
--
-- Here we reap the benefits of renaming the numerical operators. The Operator
-- precedence, in decreasing strength:
--
-- @repeated % n@, @repeated <% n@, @match - rejected@, @match ! decision@,
-- @match ?! decision@, @choice ^ (first \/ second)@.
--
-- @match - first - second@ is @(match - first) - second@.
--
-- @first & second & third@ is @first & (second & third)@. Note that @first -
-- rejected & second@ is @(first - rejected) & second@, etc.
--
-- @match \/ alternative \/ otherwise@ is @match \/ (alternative \/
-- otherwise)@. Note that @first & second \/ third@ is @(first & second) \/
-- third@.
--
-- @( match *)@, @(match +)@, @(match ?)@, @(match <?)@, @(match >?)@, @(match
-- >!)@, @(match <!)@ are the weakest and require the surrounding @()@.

infix  3 ^
infix  3 %
infix  3 <%
infix  3 !
infix  3 ?!
infixl 3 -
infixr 2 &
infixr 1 /
infix  0 ?
infix  0 *
infix  0 +
infix  0 <?
infix  0 >?
infix  0 >!

-- | @parser % n@ repeats /parser/ exactly /n/ times.
(%) :: (Match match result) => match -> Int -> Pattern
parser % n
  | n <= 0    = empty
  | otherwise = parser' *> (parser' % n .- 1)
  where
    parser' = match parser

-- | @parser <% n@ matches fewer than /n/ occurrences of /parser/.
(<%) :: (Match match result) => match -> Int -> Pattern
parser <% n = case n `compare` 1 of
  LT -> pfail "Fewer than 0 repetitions"
  EQ -> reject parser Nothing
  GT -> DeLess ^ ( ((parser ! DeLess) *> (parser <% n .- 1)) <|> empty )

data Decision = DeNone -- ""
              | DeStar -- "*"
              | DeLess -- "<%"
              | DeDirective
              | DeDoc
              | DeEscape
              | DeEscaped
              | DeFold
              | DeKey
              | DeHeader
              | DeMore
              | DeNode
              | DePair
              deriving (Show,Eq)

-- | @decision ^ (option \/ option \/ ...)@ provides a /decision/ name to the
-- choice about to be made, to allow to @commit@ to it.
(^) :: (Match match result) => Decision -> match -> Parser result
decision ^ parser = choice decision $ match parser

-- | @parser ! decision@ commits to /decision/ (in an option) after
-- successfully matching the /parser/.
(!) :: (Match match result) => match -> Decision -> Pattern
parser ! decision = match parser *> commit decision

-- | @parser ?! decision@ commits to /decision/ (in an option) if the current
-- position matches /parser/, without consuming any characters.
(?!) :: (Match match result) => match -> Decision -> Pattern
parser ?! decision = peek parser *> commit decision

-- | @lookbehind <?@ matches the current point without consuming any
-- characters, if the previous character matches the lookbehind parser (single
-- character positive lookbehind)
(<?) :: (Match match result) => match -> Parser result
(<?) lookbehind = prev lookbehind

-- | @lookahead >?@ matches the current point without consuming any characters
-- if it matches the lookahead parser (positive lookahead)
(>?) :: (Match match result) => match -> Parser result
(>?) lookahead = peek lookahead

-- | @lookahead >! matches the current point without consuming any characters
-- if it matches the lookahead parser (negative lookahead)
(>!) :: (Match match result) => match -> Pattern
(>!) lookahead = reject lookahead Nothing

-- | @parser - rejected@ matches /parser/, except if /rejected/ matches at this
-- point.
(-) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result1
parser - rejected = reject rejected Nothing *> match parser

-- | @before & after@ parses /before/ and, if it succeeds, parses /after/. This
-- basically invokes the monad's @>>=@ (bind) method.
(&) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result2
before & after = match before *> match after

-- | @first \/ second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case is fails immediately.
(/) :: (Match match1 result, Match match2 result) => match1 -> match2 -> Parser result
first / second = Parser $ applyParser (match first <|> match second)

-- | @(optional ?)@ tries to match /parser/, otherwise does nothing.
(?) :: (Match match result) => match -> Pattern
(?) optional = (match optional *> empty) <|> empty

-- | @(parser *)@ matches zero or more occurrences of /repeat/, as long as each
-- one actually consumes input characters.
(*) :: (Match match result) => match -> Pattern
(*) parser = DeStar ^ zomParser
  where
    zomParser = ((parser ! DeStar) *> match zomParser) <|> empty

-- | @(parser +)@ matches one or more occurrences of /parser/, as long as each
-- one actually consumed input characters.
(+) :: (Match match result) => match -> Pattern
(+) parser = match parser *> (parser *)

-- ** Basic parsers

-- | @first <|> second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case is fails immediately.
instance Alternative Parser where
  empty = pfail "empty"

  left <|> right = Parser $ \state -> decideParser state D.empty left right state
    where
      decideParser point tokens left right state =
        let reply = applyParser left state
            tokens' = D.append tokens $ reply^.rTokens
        in case (reply^.rResult, reply^.rCommit) of
                (Failed _,    _)      -> Reply { rState  = point,
                                                 rTokens = D.empty,
                                                 rResult = More right,
                                                 rCommit = Nothing }
                (Result _,   _)       -> reply { rTokens = tokens' }
                (More _, Just _)      -> reply { rTokens = tokens' }
                (More left', Nothing) -> decideParser point tokens' left' right (reply^.rState)


-- | @choice decision parser@ provides a /decision/ name to the choice about to
-- be made in /parser/, to allow to @commit@ to it.
choice :: Decision -> Parser result -> Parser result
choice decision parser = Parser $ \ state ->
  applyParser (choiceParser (state^.sDecision) decision parser) state { sDecision = decision }
  where choiceParser parentDecision makingDecision parser = Parser $ \ state ->
          let reply   = applyParser parser state
              commit' = case reply^.rCommit of
                             Nothing                                    -> Nothing
                             Just decision | decision == makingDecision -> Nothing
                                           | otherwise                  -> reply^.rCommit
              reply'  = case reply^.rResult of
                             More parser' -> reply { rCommit = commit',
                                                     rResult = More $ choiceParser parentDecision makingDecision parser' }
                             _            -> reply { rCommit = commit',
                                                     rState = (reply^.rState) { sDecision = parentDecision } }
          in reply'

-- | @parser ``recovery`` pattern@ parses the specified /parser/; if it fails,
-- it continues to the /recovery/ parser to recover.
recovery :: (Match match1 result) => match1 -> Parser result -> Parser result
recovery pattern recover =
  Parser $ \ state ->
    let reply = applyParser (match pattern) state
    in if state^.sIsPeek
          then reply
          else case reply^.rResult of
                    Result _       -> reply
                    More more      -> reply { rResult = More $ more `recovery` recover }
                    Failed message -> reply { rResult = More $ fake Error message *> unparsed *> recover }
    where unparsed = Parser $ \ state -> applyParser (match finishToken) $ state { sCode = Unparsed }

-- | @prev parser@ succeeds if /parser/ matches at the previous character. It
-- does not consume any input.
prev :: (Match match result) => match -> Parser result
prev parser = Parser $ \ state ->
  prevParser state (match parser) state { sIsPeek = True, sInput = (-1, state^.sLast) : state^.sInput }
  where prevParser point parser state =
          let reply = applyParser parser state
          in case reply^.rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> prevParser point parser' $ reply^.rState

-- | @peek parser@ succeeds if /parser/ matches at this point, but does not
-- consume any input.
peek :: (Match match result) => match -> Parser result
peek parser = Parser $ \ state ->
  peekParser state (match parser) state { sIsPeek = True }
  where peekParser point parser state =
          let reply = applyParser parser state
          in case reply^.rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> peekParser point parser' $ reply^.rState

-- | @reject parser name@ fails if /parser/ matches at this point, and does
-- nothing otherwise. If /name/ is provided, it is used in the error message,
-- otherwise the messages uses the current character.
reject :: (Match match result) => match -> Maybe String -> Pattern
reject parser name = Parser $ \ state ->
    rejectParser state name (match parser) state { sIsPeek = True }
  where
    rejectParser point name parser state =
      let reply = applyParser parser state
      in case reply^.rResult of
              Failed _message -> returnReply point ()
              Result _value   -> case name of
                                     Nothing   -> unexpectedReply point
                                     Just text -> failReply point $ "Unexpected " ++ text
              More parser'    -> rejectParser point name parser' $ reply^.rState

-- | @upto parser@ consumes all the character up to and not including the next
-- point where the specified parser is a match.
upto :: Pattern -> Pattern
upto parser = ( ( parser >!) *> nextIf (const True) *)

-- | @nonEmpty parser@ succeeds if /parser/ matches some non-empty input
-- characters at this point.
nonEmpty :: (Match match result) => match -> Parser result
nonEmpty parser = Parser $ \ state ->
    applyParser (nonEmptyParser (state^.sCharOffset) (match parser)) state
  where
    nonEmptyParser offset parser = Parser $ \ state ->
      let reply = applyParser parser state
          state' = reply^.rState
      in case reply^.rResult of
              Failed _message -> reply
              Result _value   -> if state'^.sCharOffset > offset
                                   then reply
                                   else failReply state' "Matched empty pattern"
              More parser'    -> reply { rResult = More $ nonEmptyParser offset parser' }

-- | @empty@ always matches without consuming any input.
empty :: Pattern
empty = return ()

-- | @eof@ matches the end of the input.
eof :: Pattern
eof = Parser $ \ state ->
  if null (state^.sInput)
     then returnReply state ()
     else unexpectedReply state

-- | @sol@ matches the start of a line.
sol :: Pattern
sol = Parser $ \ state ->
  if state^.sIsSol
     then returnReply state ()
     else failReply state "Expected start of line"

-- ** State manipulation pseudo-parsers

-- | @commit decision@ commits the parser to all the decisions up to the most
-- recent parent /decision/. This makes all tokens generated in this parsing
-- path immediately available to the caller.
commit :: Decision -> Pattern
commit decision = Parser $ \ state ->
  Reply { rState  = state,
          rTokens = D.empty,
          rResult = Result (),
          rCommit = Just decision }

-- | @nextLine@ increments @sLine@ counter and resets @sLineChar@.
nextLine :: Pattern
nextLine = Parser $ \ state ->
  returnReply state { sIsSol    = True,
                      sLine     = state^.sLine .+ 1,
                      sLineChar = 0 }
              ()

-- | @with setField getField value parser@ invokes the specified /parser/ with
-- the value of the specified field set to /value/ for the duration of the
-- invocation, using the /setField/ and /getField/ functions to manipulate it.
with :: (value -> State -> State) -> (State -> value) -> value -> Parser result -> Parser result
with setField getField value parser = Parser $ \ state ->
    let value' = getField state
        Parser parser' = value' `seq` withParser value' parser
    in parser' $ setField value state
  where
    withParser parentValue parser = Parser $ \ state ->
        let reply = applyParser parser state
        in case reply^.rResult of
             Failed _     -> reply { rState = setField parentValue $ reply^.rState }
             Result _     -> reply { rState = setField parentValue $ reply^.rState }
             More parser' -> reply { rResult = More $ withParser parentValue parser' }
{-# INLINE with #-}

-- | @parser ``forbidding`` pattern@ parses the specified /parser/ ensuring
-- that it does not contain anything matching the /forbidden/ parser.
forbidding :: (Match match1 result1) => match1 -> Parser result1 -> Parser result1
forbidding parser forbidden = with setForbidden sForbidden (Just $ forbidden *> empty) (match parser)

-- | @parser ``limitedTo`` limit@ parses the specified /parser/
-- ensuring that it does not consume more than the /limit/ input chars.
limitedTo :: (Match match result) => match -> Int -> Parser result
limitedTo parser limit = with setLimit sLimit limit (match parser)

-- ** Consuming input characters

-- | @nextIf test@ fails if the current position matches the 'State' forbidden
-- pattern or if the 'State' lookahead limit is reached. Otherwise it consumes
-- (and buffers) the next input char if it satisfies /test/.
nextIf :: (Char -> Bool) -> Pattern
nextIf test = Parser $ \ state ->
    case state^.sForbidden of
         Nothing     -> limitedNextIf state
         Just parser -> let reply = applyParser (reject parser $ Just "forbidden pattern") state { sForbidden = Nothing }
                        in case reply^.rResult of
                                Failed _ -> reply
                                Result _ -> limitedNextIf state
                                More _   -> error "unexpected Result More _ pattern"
  where
    limitedNextIf state =
        case state^.sLimit of
          -1     -> consumeNextIf state
          0      -> failReply state "Lookahead limit reached"
          _limit -> consumeNextIf state { sLimit = state^.sLimit .- 1 }

    consumeNextIf state =
        case state^.sInput of
          ((offset, char):rest) | test char -> let chars       = if state^.sIsPeek then [] else char:(state^.sChars)
                                                   byte_offset = charsOf sByteOffset sCharsByteOffset
                                                   char_offset = charsOf sCharOffset sCharsCharOffset
                                                   line        = charsOf sLine       sCharsLine
                                                   line_char   = charsOf sLineChar   sCharsLineChar
                                                   is_sol      = char == '\xFEFF' && state^.sIsSol
                                                   state' = state { sInput           = rest,
                                                                    sLast            = char,
                                                                    sChars           = chars,
                                                                    sCharsByteOffset = byte_offset,
                                                                    sCharsCharOffset = char_offset,
                                                                    sCharsLine       = line,
                                                                    sCharsLineChar   = line_char,
                                                                    sIsSol           = is_sol,
                                                                    sByteOffset      = offset,
                                                                    sCharOffset      = state^.sCharOffset .+ 1,
                                                                    sLineChar        = state^.sLineChar .+ 1 }
                                               in returnReply state' ()
                      | otherwise -> unexpectedReply state
          []                      -> unexpectedReply state
      where
        charsOf field charsField
          | state^.sIsPeek       = -1
          | null (state^.sChars) = state^.field
          | otherwise            = state^.charsField

-- ** Producing tokens

-- | @finishToken@ places all collected text into a new token and begins a new
-- one, or does nothing if there are no collected characters.
finishToken :: Pattern
finishToken = Parser $ \ state ->
  let state' = state { sChars           = [],
                       sCharsByteOffset = -1,
                       sCharsCharOffset = -1,
                       sCharsLine       = -1,
                       sCharsLineChar   = -1 }
  in if state^.sIsPeek
        then returnReply state' ()
        else case state^.sChars of
                  []          -> returnReply state' ()
                  chars@(_:_) -> tokenReply state' Token { tByteOffset = state^.sCharsByteOffset,
                                                           tCharOffset = state^.sCharsCharOffset,
                                                           tLine       = state^.sCharsLine,
                                                           tLineChar   = state^.sCharsLineChar,
                                                           tCode       = state^.sCode,
                                                           tText       = reverse chars }

-- | @wrap parser@ invokes the /parser/, ensures any unclaimed input characters
-- are wrapped into a token (only happens when testing productions), ensures no
-- input is left unparsed, and returns the parser's result.
wrap :: (Match match result) => match -> Parser result
wrap parser = do result <- match parser
                 finishToken
                 eof
                 return result

-- | @token code parser@ places all text matched by /parser/ into a 'Token' with
-- the specified /code/ (unless it is empty). Note it collects the text even if
-- there is an error.
token :: (Match match result) => Code -> match -> Pattern
token code parser = finishToken & with setCode sCode code (parser & finishToken)

-- | @fake code text@ creates a token with the specified /code/ and \"fake\"
-- /text/ characters, instead of whatever characters are collected so far.
fake :: Code -> String -> Pattern
fake code text = Parser $ \ state ->
  if state^.sIsPeek
     then returnReply state ()
     else tokenReply state Token { tByteOffset = value state sByteOffset sCharsByteOffset,
                                   tCharOffset = value state sCharOffset sCharsCharOffset,
                                   tLine       = value state sLine sCharsLine,
                                   tLineChar   = value state sLineChar sCharsLineChar,
                                   tCode       = code,
                                   tText       = text }
    where value state field1 field2 =
            if field2 state == -1
               then field1 state
               else field2 state

-- | @meta parser@ collects the text matched by the specified /parser/ into a
-- | @Meta@ token.
meta :: (Match match result) => match -> Pattern
meta parser = token Meta parser

-- | @indicator code@ collects the text matched by the specified /parser/ into an
-- @Indicator@ token.
indicator :: (Match match result) => match -> Pattern
indicator parser = token Indicator $ parser

-- | @text parser@  collects the text matched by the specified /parser/ into a
-- @Text@ token.
text :: (Match match result) => match -> Pattern
text parser = token Text parser

-- | @emptyToken code@ returns an empty token.
emptyToken :: Code -> Pattern
emptyToken code = finishToken & parser code
  where parser code = Parser $ \ state ->
          if state^.sIsPeek
             then returnReply state ()
             else tokenReply state Token { tByteOffset = state^.sByteOffset,
                                           tCharOffset = state^.sCharOffset,
                                           tLine       = state^.sLine,
                                           tLineChar   = state^.sLineChar,
                                           tCode       = code,
                                           tText       = "" }

-- | @wrapTokens beginCode endCode parser@ wraps the specified /parser/ with
-- matching /beginCode/ and /endCode/ tokens.
wrapTokens :: Code -> Code -> Pattern -> Pattern
wrapTokens beginCode endCode pattern = emptyToken beginCode
                                      & prefixErrorWith pattern (emptyToken endCode)
                                      & emptyToken endCode

-- | @prefixErrorWith pattern prefix@ will invoke the @prefix@ parser if an
-- error is detected during the @pattern@ parser, and then return the error.
prefixErrorWith :: (Match match result) => match -> Pattern -> Parser result
prefixErrorWith pattern prefix =
  Parser $ \ state ->
    let reply = applyParser (match pattern) state
    in case reply^.rResult of
         Result _       -> reply
         More more      -> reply { rResult = More $ prefixErrorWith more prefix }
         Failed message -> reply { rResult = More $ prefix & (pfail message :: Parser result) }

-- * Production parameters

-- | Production context.
data Context = BlockOut     -- ^ Outside block sequence.
             | BlockIn      -- ^ Inside block sequence.
             | FlowOut      -- ^ Outside flow collection.
             | FlowIn       -- ^ Inside flow collection.
             | BlockKey     -- ^ Implicit block key.
             | FlowKey      -- ^ Implicit flow key.

{-
-- | @show context@ converts a 'Context' to a 'String'.
instance Show Context where
  show context = case context of
                      BlockOut -> "block-out"
                      BlockIn  -> "block-in"
                      FlowOut  -> "flow-out"
                      FlowIn   -> "flow-in"
                      BlockKey -> "block-key"
                      FlowKey  -> "flow-key"

-- | @read context@ converts a 'String' to a 'Context'. We trust our callers to
-- convert any @-@ characters into @_@ to allow the built-in @lex@ function to
-- handle the names as single identifiers.
instance Read Context where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "block_out" -> BlockOut
                        "block_in"  -> BlockIn
                        "flow_out"  -> FlowOut
                        "flow_in"   -> FlowIn
                        "block_key" -> BlockKey
                        "flow_key"  -> FlowKey
                        _           -> error $ "unknown context: " ++ word
-}

-- | Chomp method.
data Chomp = Strip -- ^ Remove all trailing line breaks.
           | Clip  -- ^ Keep first trailing line break.
           | Keep  -- ^ Keep all trailing line breaks.

{-
-- | @show chomp@ converts a 'Chomp' to a 'String'.
instance Show Chomp where
  show chomp = case chomp of
                    Strip -> "strip"
                    Clip  -> "clip"
                    Keep  -> "keep"

-- | @read chomp@ converts a 'String' to a 'Chomp'.
instance Read Chomp where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "strip" -> Strip
                        "clip"  -> Clip
                        "keep"  -> Keep
                        _       -> error $ "unknown chomp: " ++ word
-}

-- * Tokenizers
--
-- We encapsulate the 'Data.YAML.Token.Parser' inside a 'Tokenizer'. This allows us to hide the
-- implementation details from our callers.

-- | 'Tokenizer' converts a input text into a list of 'Token'. Errors
-- are reported as tokens with the @Error@ 'Code', and the unparsed text
-- following an error may be attached as a final token (if the @Bool@ is
-- @True@). Note that tokens are available \"immediately\", allowing for
-- streaming of large YAML files with memory requirements depending only on the
-- YAML nesting level.
type Tokenizer = BLC.ByteString -> Bool -> [Token]

-- | @patternTokenizer pattern@ converts the /pattern/ to a simple 'Tokenizer'.
patternTokenizer :: Pattern -> Tokenizer
patternTokenizer pattern input withFollowing =
    D.toList $ patternParser (wrap pattern) (initialState input)
  where
    patternParser parser state =
      let reply = applyParser parser state
          tokens = commitBugs reply
          state' = reply^.rState
      in case reply^.rResult of
              Failed message -> errorTokens tokens state' message withFollowing
              Result _       -> tokens
              More parser'   -> D.append tokens $ patternParser parser' state'

-- | @errorTokens tokens state message withFollowing@ appends an @Error@ token
-- with the specified /message/ at the end of /tokens/, and if /withFollowing/
-- also appends the unparsed text following the error as a final @Unparsed@
-- token.
errorTokens :: D.DList Token -> State -> String -> Bool -> D.DList Token
errorTokens tokens state message withFollowing =
    let tokens' = D.append tokens $ D.singleton Token { tByteOffset = state^.sByteOffset,
                                                        tCharOffset = state^.sCharOffset,
                                                        tLine       = state^.sLine,
                                                        tLineChar   = state^.sLineChar,
                                                        tCode       = Error,
                                                        tText       = message }
    in if withFollowing && state^.sInput /= []
       then D.append tokens' $ D.singleton Token { tByteOffset = state^.sByteOffset,
                                                   tCharOffset = state^.sCharOffset,
                                                   tLine       = state^.sLine,
                                                   tLineChar   = state^.sLineChar,
                                                   tCode       = Unparsed,
                                                   tText       = map snd $ state^.sInput }
       else tokens'

-- | @commitBugs reply@ inserts an error token if a commit was made outside a
-- named choice. This should never happen outside tests.
commitBugs :: Reply result -> D.DList Token
commitBugs reply =
  let tokens = reply^.rTokens
      state = reply^.rState
  in case reply^.rCommit of
          Nothing     -> tokens
          Just commit -> D.append tokens $ D.singleton Token { tByteOffset = state^.sByteOffset,
                                                               tCharOffset = state^.sCharOffset,
                                                               tLine       = state^.sLine,
                                                               tLineChar   = state^.sLineChar,
                                                               tCode       = Error,
                                                               tText       = "Commit to " ++ show commit ++ " was made outside it" }

-- | @'tokenize' input emit_unparsed@ converts the Unicode /input/
-- (using the UTF-8, UTF-16 (LE or BE), or UTF-32 (LE or BE) encoding)
-- to a list of 'Token' according to the YAML 1.2 specification.
--
-- Errors are reported as tokens with @'Error' :: 'Code'@, and the
-- unparsed text following an error may be attached as a final 'Unparsed' token
-- (if the /emit_unparsed/ argument is @True@). Note that tokens are available
-- \"immediately\", allowing for streaming of large YAML files with
-- memory requirements depending only on the YAML nesting level.
tokenize :: BLC.ByteString -> Bool -> [Token]
tokenize = patternTokenizer l_yaml_stream

-- * Productions

-- ** BNF compatibility helpers

-- | @detect_utf_encoding@ doesn't actually detect the encoding, we just call it
-- this way to make the productions compatible with the spec. Instead it simply
-- reports the encoding (which was already detected when we started parsing).
bom :: Match match1 result1 => match1 -> Parser ()
bom code = code
         & (Parser $ \ state -> let text = case state^.sEncoding of
                                                UTF8    -> "TF-8"
                                                UTF16LE -> "TF-16LE"
                                                UTF16BE -> "TF-16BE"
                                                UTF32LE -> "TF-32LE"
                                                UTF32BE -> "TF-32BE"
                                in applyParser (fake Bom text) state)

-- | @na@ is the \"non-applicable\" indentation value. We use Haskell's laziness
-- to verify it really is never used.
na :: Int
na = error "Accessing non-applicable indentation"

-- | @asInteger@ returns the last consumed character, which is assumed to be a
-- decimal digit, as an integer.
asInteger :: Parser Int
asInteger = Parser $ \ state -> returnReply state $ ord (state^.sLast) .- 48

-- | @result value@ is the same as /return value/ except that we give the
-- Haskell type deduction the additional boost it needs to figure out this is
-- wrapped in a 'Data.YAML.Token.Parser'.
result :: result -> Parser result
result = return

----------------------------------------------------------------------------

-- ** Spec productions
--
-- These are copied directly from the spec, with the sprinkling of
-- additional token and decision point directives.

-- 5.1 Character Set

c_printable {- 1 -} = '\x9' / '\xA' / '\xD' / ('\x20', '\x7E')
                    / '\x85' / ('\xA0', '\xD7FF') / ('\xE000', '\xFFFD')
                    / ('\x10000', '\x10FFFF')

nb_json {- 2 -} = '\x9' / ('\x20', '\x10FFFF')

-- 5.2 Character Encodings

c_byte_order_mark {- 3 -} = bom '\xFEFF'

-- 5.3 Indicator Characters

c_sequence_entry {-  4 -} = indicator '-'
c_mapping_key    {-  5 -} = indicator '?'
c_mapping_value  {-  6 -} = indicator ':'

c_collect_entry  {-  7 -} = indicator ','
c_sequence_start {-  8 -} = indicator '['
c_sequence_end   {-  9 -} = indicator ']'
c_mapping_start  {- 10 -} = indicator '{'
c_mapping_end    {- 11 -} = indicator '}'

c_comment        {- 12 -} = indicator '#'

c_anchor         {- 13 -} = indicator '&'
c_alias          {- 14 -} = indicator '*'
c_tag            {- 15 -} = indicator '!'

c_literal        {- 16 -} = indicator '|'
c_folded         {- 17 -} = indicator '>'

c_single_quote   {- 18 -} = indicator '\''
c_double_quote   {- 19 -} = indicator '"'

c_directive      {- 20 -} = indicator '%'

c_reserved       {- 21 -} = indicator ( '@' /  '`' )

c_indicator {- 22 -} = c_sequence_entry / c_mapping_key / c_mapping_value / c_collect_entry
                     / c_sequence_start / c_sequence_end / c_mapping_start / c_mapping_end
                     / c_comment / c_anchor / c_alias / c_tag
                     / c_literal / c_folded / c_single_quote / c_double_quote
                     / c_directive / c_reserved

c_flow_indicator {- 23 -} = c_collect_entry / c_sequence_start / c_sequence_end / c_mapping_start / c_mapping_end

-- 5.4 Line Break Characters

b_line_feed       {- 24 -} = '\xA'
b_carriage_return {- 25 -} = '\xD'
b_char            {- 26 -} = b_line_feed / b_carriage_return

nb_char {- 27 -} = c_printable - b_char - c_byte_order_mark

b_break {- 28 -} = ( b_carriage_return & b_line_feed
                   / b_carriage_return
                   / b_line_feed )
                 & nextLine

b_as_line_feed {- 29 -} = token LineFeed b_break

b_non_content {- 30 -} = token Break b_break

-- 5.5 White Space Characters

s_space {- 31 -} = '\x20'
s_tab   {- 32 -} = '\x9'
s_white {- 33 -} = s_space / s_tab

ns_char {- 34 -} = nb_char - s_white

-- 5.6 Miscellaneous Characters

ns_dec_digit {- 35 -} = ('\x30', '\x39')

ns_hex_digit {- 36 -} = ns_dec_digit
                      / ('\x41', '\x46') / ('\x61', '\x66')

ns_ascii_letter {- 37 -} = ('\x41', '\x5A') / ('\x61', '\x7A')

ns_word_char {- 38 -} = ns_dec_digit / ns_ascii_letter / '-'

ns_uri_char {- 39 -} = DeEscape
                     ^ ( '%' ! DeEscape & ns_hex_digit & ns_hex_digit / ns_word_char / '#'
                       / ';' / '/' / '?' / ':' / '@' / '&'  / '=' / '+' / '$' / ','
                       / '_' / '.' / '!' / '~' / '*' / '\'' / '(' / ')' / '[' / ']' )

ns_tag_char {- 40 -} = ns_uri_char - c_tag - c_flow_indicator

-- 5.7 Escaped Characters

c_escape {- 41 -} = indicator '\\'

ns_esc_null                {- 42 -} = meta '0'
ns_esc_bell                {- 43 -} = meta 'a'
ns_esc_backspace           {- 44 -} = meta 'b'
ns_esc_horizontal_tab      {- 45 -} = meta ( 't' / '\x9' )
ns_esc_line_feed           {- 46 -} = meta 'n'
ns_esc_vertical_tab        {- 47 -} = meta 'v'
ns_esc_form_feed           {- 48 -} = meta 'f'
ns_esc_carriage_return     {- 49 -} = meta 'r'
ns_esc_escape              {- 50 -} = meta 'e'
ns_esc_space               {- 51 -} = meta '\x20'
ns_esc_double_quote        {- 52 -} = meta '"'
ns_esc_slash               {- 53 -} = meta '/'
ns_esc_backslash           {- 54 -} = meta '\\'
ns_esc_next_line           {- 55 -} = meta 'N'
ns_esc_non_breaking_space  {- 56 -} = meta '_'
ns_esc_line_separator      {- 57 -} = meta 'L'
ns_esc_paragraph_separator {- 58 -} = meta 'P'
ns_esc_8_bit               {- 59 -} = indicator 'x' ! DeEscaped & meta ( ns_hex_digit % 2 )
ns_esc_16_bit              {- 60 -} = indicator 'u' ! DeEscaped & meta ( ns_hex_digit % 4 )
ns_esc_32_bit              {- 61 -} = indicator 'U' ! DeEscaped & meta ( ns_hex_digit % 8 )

c_ns_esc_char {- 62 -} = wrapTokens BeginEscape EndEscape
                       $ c_escape ! DeEscape
                       & DeEscaped
                       ^ ( ns_esc_null / ns_esc_bell / ns_esc_backspace
                         / ns_esc_horizontal_tab / ns_esc_line_feed
                         / ns_esc_vertical_tab / ns_esc_form_feed
                         / ns_esc_carriage_return / ns_esc_escape / ns_esc_space
                         / ns_esc_double_quote / ns_esc_slash / ns_esc_backslash
                         / ns_esc_next_line / ns_esc_non_breaking_space
                         / ns_esc_line_separator / ns_esc_paragraph_separator
                         / ns_esc_8_bit / ns_esc_16_bit / ns_esc_32_bit )

-- 6.1 Indentation Spaces

s_indent    n {- 63 -} = token Indent ( s_space % n )

s_indent_lt n {- 64 -} = token Indent ( s_space <% n )
s_indent_le n {- 65 -} = token Indent ( s_space <% (n .+ 1) )

-- 6.2 Separation Spaces

s_separate_in_line {- 66 -} = token White ( s_white +) / sol

-- 6.3 Line Prefixes

s_line_prefix n c {- 67 -} = case c of
                                  BlockOut -> s_block_line_prefix n
                                  BlockIn  -> s_block_line_prefix n
                                  FlowOut  -> s_flow_line_prefix n
                                  FlowIn   -> s_flow_line_prefix n
                                  _        -> error "unexpected node style pattern in s_line_prefix"

s_block_line_prefix n {- 68 -} = s_indent n
s_flow_line_prefix  n {- 69 -} = s_indent n & ( s_separate_in_line ?)

-- 6.4 Empty Lines

l_empty n c {- 70 -} = ( s_line_prefix n c / s_indent_lt n )
                     & b_as_line_feed

-- 6.5 Line Folding

b_l_trimmed  n c {- 71 -} = b_non_content & ( l_empty n c +)

b_as_space {- 72 -} = token LineFold b_break

b_l_folded n c  {- 73 -} = b_l_trimmed n c / b_as_space

s_flow_folded n {- 74 -} = ( s_separate_in_line ?) & b_l_folded n FlowIn
                         & s_flow_line_prefix n

-- 6.6 Comments

c_nb_comment_text {- 75 -} = wrapTokens BeginComment EndComment
                           $ c_comment & meta ( nb_char *)

b_comment {- 76 -} = b_non_content / eof

s_b_comment {- 77 -} = ( s_separate_in_line & ( c_nb_comment_text ?) ?)
                     & b_comment

l_comment {- 78 -} = s_separate_in_line & ( c_nb_comment_text ?) & b_comment

s_l_comments {- 79 -} = ( s_b_comment / sol )
                      & ( nonEmpty l_comment *)

-- 6.7 Separation Lines

s_separate n c      {- 80 -} = case c of
                                    BlockOut -> s_separate_lines n
                                    BlockIn  -> s_separate_lines n
                                    FlowOut  -> s_separate_lines n
                                    FlowIn   -> s_separate_lines n
                                    BlockKey -> s_separate_in_line
                                    FlowKey  -> s_separate_in_line
s_separate_lines n {- 81 -} = s_l_comments & s_flow_line_prefix n
                            / s_separate_in_line

-- 6.8 Directives

l_directive {- 82 -} = ( wrapTokens BeginDirective EndDirective
                       $ c_directive ! DeDoc
                       & DeDirective
                       ^ ( ns_yaml_directive
                         / ns_tag_directive
                         / ns_reserved_directive ) )
                     & s_l_comments

ns_reserved_directive  {- 83 -} = ns_directive_name
                                & ( s_separate_in_line & ns_directive_parameter *)
ns_directive_name      {- 84 -} = meta ( ns_char +)
ns_directive_parameter {- 85 -} = meta ( ns_char +)

-- 6.8.1 Yaml Directives

ns_yaml_directive {- 86 -} = meta [ 'Y', 'A', 'M', 'L' ] ! DeDirective
                           & s_separate_in_line & ns_yaml_version
ns_yaml_version   {- 87 -} = meta ( ( ns_dec_digit +) & '.' & ( ns_dec_digit +) )

-- 6.8.2 Tag Directives

ns_tag_directive {- 88 -} = meta [ 'T', 'A', 'G' ] ! DeDirective
                          & s_separate_in_line & c_tag_handle
                          & s_separate_in_line & ns_tag_prefix

-- 6.8.2.1 Tag Handles

c_tag_handle {- 89 -} = c_named_tag_handle
                      / c_secondary_tag_handle
                      / c_primary_tag_handle

c_primary_tag_handle   {- 90 -} = wrapTokens BeginHandle EndHandle
                                $ c_tag

c_secondary_tag_handle {- 91 -} = wrapTokens BeginHandle EndHandle
                                $ c_tag & c_tag

c_named_tag_handle     {- 92 -} = wrapTokens BeginHandle EndHandle
                                $ c_tag & meta ( ns_word_char +) & c_tag

-- 6.8.2.2 Tag Prefixes

ns_tag_prefix {- 93 -} = wrapTokens BeginTag EndTag
                       $ ( c_ns_local_tag_prefix / ns_global_tag_prefix )

c_ns_local_tag_prefix {- 94 -} = c_tag & meta ( ns_uri_char *)

ns_global_tag_prefix  {- 95 -} = meta ( ns_tag_char & ( ns_uri_char *) )

-- 6.9 Node Properties

c_ns_properties n c {- 96 -} = wrapTokens BeginProperties EndProperties
                             $ ( c_ns_tag_property
                               & ( s_separate n c & c_ns_anchor_property ?) )
                             / ( c_ns_anchor_property
                               & ( s_separate n c & c_ns_tag_property ?) )

-- 6.9.1 Node Tags

c_ns_tag_property {- 97 -} = wrapTokens BeginTag EndTag
                           $ c_verbatim_tag
                           / c_ns_shorthand_tag
                           / c_non_specific_tag

c_verbatim_tag     {- 98 -} = c_tag & indicator '<' & meta ( ns_uri_char +) & indicator '>'

c_ns_shorthand_tag {- 99 -} = c_tag_handle & meta ( ns_tag_char +)

c_non_specific_tag {- 100 -} = c_tag

-- 6.9.2 Node Anchors

c_ns_anchor_property {- 101 -} = wrapTokens BeginAnchor EndAnchor
                               $ c_anchor & ns_anchor_name

ns_anchor_char {- 102 -} = ns_char - c_flow_indicator
ns_anchor_name {- 103 -} = meta ( ns_anchor_char +)

-- 7.1 Alias Nodes

c_ns_alias_node {- 104 -} = wrapTokens BeginAlias EndAlias
                          $ c_alias ! DeNode & ns_anchor_name

-- 7.2 Empty Nodes

e_scalar {- 105 -} = wrapTokens BeginScalar EndScalar empty

e_node {- 106 -} = wrapTokens BeginNode EndNode e_scalar

-- 7.3.1 Double Quoted Style

nb_double_char {- 107 -} = DeEscape ^ ( c_ns_esc_char / ( nb_json - c_escape - c_double_quote ) )
ns_double_char {- 108 -} = nb_double_char - s_white

c_double_quoted n c {- 109 -} = wrapTokens BeginScalar EndScalar
                              $ c_double_quote ! DeNode & text ( nb_double_text n c ) & c_double_quote
nb_double_text n c  {- 110 -} = case c of
                                     FlowOut  -> nb_double_multi_line n
                                     FlowIn   -> nb_double_multi_line n
                                     BlockKey -> nb_double_one_line
                                     FlowKey  -> nb_double_one_line
                                     _        -> error "unexpected node style pattern in nb_double_text"

nb_double_one_line  {- 111 -} = ( nb_double_char *)

s_double_escaped n {- 112 -} = ( s_white *)
                             & wrapTokens BeginEscape EndEscape ( c_escape ! DeEscape & b_non_content )
                             & ( l_empty n FlowIn *)
                             & s_flow_line_prefix n
s_double_break n   {- 113 -} = DeEscape ^ ( s_double_escaped n / s_flow_folded n )

nb_ns_double_in_line    {- 114 -} = ( ( s_white *) & ns_double_char *)
s_double_next_line n {- 115 -} = s_double_break n
                               & ( ns_double_char & nb_ns_double_in_line
                                 & ( s_double_next_line n / ( s_white *) ) ?)
nb_double_multi_line n  {- 116 -} = nb_ns_double_in_line
                                  & ( s_double_next_line n / ( s_white *) )

-- 7.3.2 Single Quoted Style

c_quoted_quote {- 117 -} = wrapTokens BeginEscape EndEscape
                         $ c_single_quote ! DeEscape & meta '\''
nb_single_char {- 118 -} = DeEscape ^ ( c_quoted_quote / ( nb_json - c_single_quote ) )
ns_single_char {- 119 -} = nb_single_char - s_white

c_single_quoted  n c {- 120 -} = wrapTokens BeginScalar EndScalar
                               $ c_single_quote ! DeNode & text ( nb_single_text n c ) & c_single_quote
nb_single_text n c {- 121 -} = case c of
                                    FlowOut  -> nb_single_multi_line n
                                    FlowIn   -> nb_single_multi_line n
                                    BlockKey -> nb_single_one_line
                                    FlowKey  -> nb_single_one_line
                                    _        -> error "unexpected node style pattern in nb_single_text"

nb_single_one_line {- 122 -} = ( nb_single_char *)

nb_ns_single_in_line    {- 123 -} = ( ( s_white *) & ns_single_char *)
s_single_next_line n {- 124 -} = s_flow_folded n
                                 & ( ns_single_char & nb_ns_single_in_line
                                    & ( s_single_next_line n / ( s_white *) ) ?)
nb_single_multi_line n  {- 125 -} = nb_ns_single_in_line
                                  & ( s_single_next_line n / ( s_white *) )

-- 7.3.3 Plain Style

ns_plain_first _c  {- 126 -} = ns_char - c_indicator
                            / ( ':' / '?' / '-' ) & ( (ns_plain_safe _c) >?)

ns_plain_safe c   {- 127 -} = case c of
                                   FlowOut  -> ns_plain_safe_out
                                   FlowIn   -> ns_plain_safe_in
                                   BlockKey -> ns_plain_safe_out
                                   FlowKey  -> ns_plain_safe_in
                                   _        -> error "unexpected node style pattern in ns_plain_safe"

ns_plain_safe_out {- 128 -} = ns_char
ns_plain_safe_in  {- 129 -} = ns_char - c_flow_indicator
ns_plain_char c   {- 130 -} = ns_plain_safe c - ':' - '#'
                            / ( ns_char <?) & '#'
                            / ':' & ( (ns_plain_safe c) >?)

ns_plain n c          {- 131 -} = wrapTokens BeginScalar EndScalar
                                $ text (case c of
                                             FlowOut  -> ns_plain_multi_line n c
                                             FlowIn   -> ns_plain_multi_line n c
                                             BlockKey -> ns_plain_one_line c
                                             FlowKey  -> ns_plain_one_line c
                                             _        -> error "unexpected node style pattern in ns_plain")

nb_ns_plain_in_line c {- 132 -} = ( ( s_white *) & ns_plain_char c *)
ns_plain_one_line c   {- 133 -} = ns_plain_first c ! DeNode & nb_ns_plain_in_line c

s_ns_plain_next_line n c {- 134 -} = s_flow_folded n
                                   & ns_plain_char c & nb_ns_plain_in_line c
ns_plain_multi_line n c  {- 135 -} = ns_plain_one_line c
                                   & ( s_ns_plain_next_line n c *)

-- 7.4 Flow Collection Styles

in_flow c {- 136 -} = case c of
                           FlowOut  -> FlowIn
                           FlowIn   -> FlowIn
                           BlockKey -> FlowKey
                           FlowKey  -> FlowKey
                           _        -> error "unexpected node style pattern in in_flow"

-- 7.4.1 Flow Sequences

c_flow_sequence n c {- 137 -} = wrapTokens BeginSequence EndSequence
                              $ c_sequence_start ! DeNode & ( s_separate n c ?)
                              & ( ns_s_flow_seq_entries n (in_flow c) ?) & c_sequence_end

ns_s_flow_seq_entries n c {- 138 -} = ns_flow_seq_entry n c & ( s_separate n c ?)
                                    & ( c_collect_entry & ( s_separate n c ?)
                                      & ( ns_s_flow_seq_entries n c ?) ?)

ns_flow_seq_entry n c {- 139 -} = DePair ^ ( ns_flow_pair n c / DeNode ^ ns_flow_node n c )

-- 7.4.2 Flow Mappings

c_flow_mapping n c        {- 140 -} = wrapTokens BeginMapping EndMapping
                                    $ c_mapping_start ! DeNode & ( s_separate n c ?)
                                    & ( ns_s_flow_map_entries n (in_flow c) ?) & c_mapping_end
ns_s_flow_map_entries n c {- 141 -} = ns_flow_map_entry n c & ( s_separate n c ?)
                                    & ( c_collect_entry & ( s_separate n c ?)
                                      & ( ns_s_flow_map_entries n c ?) ?)

ns_flow_map_entry n c {- 142 -}          = wrapTokens BeginPair EndPair
                                         $ DeKey ^ ( ( c_mapping_key ! DeKey & s_separate n c
                                                     & ns_flow_map_explicit_entry n c )
                                                   / ns_flow_map_implicit_entry n c )
ns_flow_map_explicit_entry n c {- 143 -} = ns_flow_map_implicit_entry n c
                                         / ( e_node
                                           & e_node )

ns_flow_map_implicit_entry n c {- 144 -}    = DePair
                                            ^ ( c_ns_flow_map_json_key_entry n c
                                              / ns_flow_map_yaml_key_entry n c
                                              / c_ns_flow_map_empty_key_entry n c )
ns_flow_map_yaml_key_entry n c {- 145 -}    = ( DeNode ^ ns_flow_yaml_node n c ) ! DePair
                                            & ( ( ( s_separate n c ?)
                                                & c_ns_flow_map_separate_value n c )
                                              / e_node )
c_ns_flow_map_empty_key_entry n c {- 146 -} = e_node
                                            & c_ns_flow_map_separate_value n c

c_ns_flow_map_separate_value n c {- 147 -}  = c_mapping_value & ( (ns_plain_safe c) >!) ! DePair
                                            & ( ( s_separate n c & ns_flow_node n c )
                                              / e_node )

c_ns_flow_map_json_key_entry n c {- 148 -} = ( DeNode ^ c_flow_json_node n c ) ! DePair
                                           & ( ( ( s_separate n c ?)
                                               & c_ns_flow_map_adjacent_value n c )
                                             / e_node )
c_ns_flow_map_adjacent_value n c {- 149 -} = c_mapping_value ! DePair
                                           & ( ( ( s_separate n c ?)
                                               & ns_flow_node n c )
                                               / e_node )

ns_flow_pair n c {- 150 -} = wrapTokens BeginMapping EndMapping
                           $ wrapTokens BeginPair EndPair
                           $ ( ( c_mapping_key ! DePair & s_separate n c
                               & ns_flow_map_explicit_entry n c )
                             / ns_flow_pair_entry n c )

ns_flow_pair_entry n c            {- 151 -} = ( ns_flow_pair_yaml_key_entry n c
                                              / c_ns_flow_map_empty_key_entry n c
                                              / c_ns_flow_pair_json_key_entry n c )
ns_flow_pair_yaml_key_entry n c   {- 152 -} = ns_s_implicit_yaml_key FlowKey
                                            & c_ns_flow_map_separate_value n c
c_ns_flow_pair_json_key_entry n c {- 153 -} = c_s_implicit_json_key FlowKey
                                            & c_ns_flow_map_adjacent_value n c
ns_s_implicit_yaml_key c          {- 154 -} = ( DeNode ^ ( ns_flow_yaml_node na c ) & ( s_separate_in_line ?) )
                                            `limitedTo` 1024
c_s_implicit_json_key c           {- 155 -} = ( DeNode ^ ( c_flow_json_node  na c ) & ( s_separate_in_line ?) )
                                            `limitedTo` 1024

-- 7.5 Flow Nodes

ns_flow_yaml_content n c {- 156 -} = ns_plain n c
c_flow_json_content n c  {- 157 -} = c_flow_sequence n c / c_flow_mapping n c
                                   / c_single_quoted n c / c_double_quoted n c
ns_flow_content n c      {- 158 -} = ns_flow_yaml_content n c / c_flow_json_content n c

ns_flow_yaml_node n c {- 159 -} = wrapTokens BeginNode EndNode
                                $ c_ns_alias_node
                                / ns_flow_yaml_content n c
                                / ( c_ns_properties n c
                                  & ( ( s_separate n c & ns_flow_yaml_content n c )
                                    / e_scalar ) )
c_flow_json_node n c  {- 160 -} = wrapTokens BeginNode EndNode
                                $ ( c_ns_properties n c & s_separate n c ?)
                                & c_flow_json_content n c
ns_flow_node n c      {- 161 -} = wrapTokens BeginNode EndNode
                                $ c_ns_alias_node
                                / ns_flow_content n c
                                / ( c_ns_properties n c
                                  & ( ( s_separate n c & ns_flow_content n c )
                                    / e_scalar ) )

-- 8.1.1 Block Scalar Headers

c_b_block_header n {- 162 -} = DeHeader
                             ^ ( do m <- c_indentation_indicator n
                                    t <- c_chomping_indicator
                                    ( s_white / b_char ) ?! DeHeader
                                    s_b_comment
                                    result (m, t)
                               / do t <- c_chomping_indicator
                                    m <- c_indentation_indicator n
                                    s_b_comment
                                    result (m, t) )

-- 8.1.1.1 Block Indentation Indicator

c_indentation_indicator n {- 163 -} = fmap fixup (indicator ( ns_dec_digit - '0' ) & asInteger)
                                    / detect_scalar_indentation n
  where
    fixup | n == -1   = (.+ 1) -- compensate for anomaly at left-most n
          | otherwise = id

detect_scalar_indentation n = peek $ ( nb_char *)
                              -- originally:
                              --   & ( b_non_content & ( l_empty n BlockIn *) ?)
                                   & ( b_break & ( (s_space *) & b_break *) ?)
                                   & count_spaces (-n)

count_spaces n  = (s_space & count_spaces (n .+ 1))
                / result (max 1 n)

-- 8.1.1.2 Chomping Indicator

c_chomping_indicator {- 164 -} = indicator '-' & result Strip
                               / indicator '+' & result Keep
                               / result Clip

end_block_scalar t = case t of
                          Strip -> emptyToken EndScalar
                          Clip  -> emptyToken EndScalar
                          Keep  -> empty

b_chomped_last t {- 165 -} = case t of
                                  Strip -> emptyToken EndScalar & b_non_content
                                  Clip  -> b_as_line_feed & emptyToken EndScalar
                                  Keep  -> b_as_line_feed

l_chomped_empty n t {- 166 -} = case t of
                                     Strip -> l_strip_empty n
                                     Clip  -> l_strip_empty n
                                     Keep  -> l_keep_empty n
l_strip_empty n     {- 167 -} = ( s_indent_le n & b_non_content *)
                              & ( l_trail_comments n ?)
l_keep_empty n      {- 168 -} = ( l_empty n BlockIn *)
                              & emptyToken EndScalar
                              & ( l_trail_comments n ?)

l_trail_comments n {- 169 -} = s_indent_lt n & c_nb_comment_text & b_comment
                             & ( nonEmpty l_comment *)

-- 8.1.2 Literal Style

c_l__literal n {- 170 -} = do emptyToken BeginScalar
                              c_literal ! DeNode
                              (m, t) <- c_b_block_header n `prefixErrorWith` emptyToken EndScalar
                              text ( l_literal_content (n .+ m) t )

l_nb_literal_text n   {- 171 -} = ( l_empty n BlockIn *)
                                & s_indent n & ( nb_char +)
b_nb_literal_next n   {- 172 -} = b_as_line_feed
                                & l_nb_literal_text n
l_literal_content n t {- 173 -} = ( ( l_nb_literal_text n & ( b_nb_literal_next n *) & b_chomped_last t )
                                  / end_block_scalar t )
                                & l_chomped_empty n t

-- 8.1.3 Folded Style

c_l__folded n {- 174 -} = do emptyToken BeginScalar
                             c_folded ! DeNode
                             (m, t) <- c_b_block_header n `prefixErrorWith` emptyToken EndScalar
                             text ( l_folded_content (n .+ m) t )

s_nb_folded_text n  {- 175 -} = s_indent n & ns_char ! DeFold & ( nb_char *)
l_nb_folded_lines n {- 176 -} = s_nb_folded_text n
                              & ( b_l_folded n BlockIn & s_nb_folded_text n *)

s_nb_spaced_text n  {- 177 -} = s_indent n & s_white ! DeFold & ( nb_char *)
b_l_spaced        n {- 178 -} = b_as_line_feed
                              & ( l_empty n BlockIn *)
l_nb_spaced_lines n {- 179 -} = s_nb_spaced_text n
                              & ( b_l_spaced n & s_nb_spaced_text n *)

l_nb_same_lines n {- 180 -} = ( l_empty n BlockIn *)
                            & DeFold ^ ( l_nb_folded_lines n / l_nb_spaced_lines n )

l_nb_diff_lines n {- 181 -} = l_nb_same_lines n
                            & ( b_as_line_feed & l_nb_same_lines n *)

l_folded_content n t {- 182 -} = ( ( l_nb_diff_lines n & b_chomped_last t )
                                 / end_block_scalar t )
                               & l_chomped_empty n t

-- 8.2.1 Block Sequences

detect_collection_indentation n = peek $ ( nonEmpty l_comment* ) & count_spaces (-n)
detect_inline_indentation       = peek $ count_spaces 0

l__block_sequence n   {- 183 -} = do m  <- detect_collection_indentation n
                                     wrapTokens BeginSequence EndSequence $ ( s_indent (n .+ m) & c_l_block_seq_entry (n .+ m) +)
c_l_block_seq_entry n {- 184 -} = c_sequence_entry & ( ns_char >!) ! DeNode
                                & s_l__block_indented n BlockIn

s_l__block_indented n c {- 185 -} = do m <- detect_inline_indentation
                                       DeNode ^ ( ( s_indent m
                                                  & ( ns_l_in_line_sequence (n .+ 1 .+ m)
                                                    / ns_l_in_line_mapping (n .+ 1 .+ m) ) )
                                                / s_l__block_node n c
                                                / ( e_node & ( s_l_comments ?) & unparsed (n .+ 1) ) ) `recovery` unparsed (n .+ 1)
ns_l_in_line_sequence n {- 186 -} = wrapTokens BeginNode EndNode
                                  $ wrapTokens BeginSequence EndSequence
                                  $ c_l_block_seq_entry n
                                  & ( s_indent n & c_l_block_seq_entry n *)

-- 8.2.2 Block Mappings

l__block_mapping n = {- 187 -} do m <- detect_collection_indentation n
                                  wrapTokens BeginMapping EndMapping $ ( s_indent (n .+ m) & ns_l_block_map_entry (n .+ m) +)

ns_l_block_map_entry n {- 188 -} = wrapTokens BeginPair EndPair
                                 $ c_l_block_map_explicit_entry n
                                 / ns_l_block_map_implicit_entry n
c_l_block_map_explicit_entry n {- 189 -} = c_l_block_map_explicit_key n
                                         & ( l_block_map_explicit_value n
                                         / e_node )
c_l_block_map_explicit_key n   {- 190 -} = c_mapping_key & ( ns_char >!) ! DeNode & s_l__block_indented n BlockOut
l_block_map_explicit_value n   {- 191 -} = s_indent n & c_mapping_value & s_l__block_indented n BlockOut

ns_l_block_map_implicit_entry n {- 192 -} = ( ns_s_block_map_implicit_key
                                            / e_node )
                                          & c_l_block_map_implicit_value n
ns_s_block_map_implicit_key     {- 193 -} = c_s_implicit_json_key BlockKey
                                          / ns_s_implicit_yaml_key BlockKey

c_l_block_map_implicit_value n  {- 194 -} = c_mapping_value ! DeNode
                                          & ( ( s_l__block_node n BlockOut
                                              / ( e_node & ( s_l_comments ?) & unparsed (n .+ 1) ) ) `recovery` unparsed (n .+ 1) )

ns_l_in_line_mapping n {- 195 -} = wrapTokens BeginNode EndNode
                                 $ wrapTokens BeginMapping EndMapping
                                 $ ns_l_block_map_entry n
                                 & ( s_indent n & ns_l_block_map_entry n *)

-- 8.2.3 Block Nodes

unparsed n = ( sol / unparsed_text & unparsed_break )
           & ( nonEmpty ( unparsed_indent n & unparsed_text & unparsed_break ) *)
unparsed_indent n = token Unparsed ( s_space % n )
unparsed_text = token Unparsed ( upto ( eof / c_forbidden / b_break ) )
unparsed_break = eof / peek c_forbidden / token Unparsed b_break / empty

s_l__block_node n c  {- 196 -} = s_l__block_in_block n c / s_l__flow_in_block n
s_l__flow_in_block n {- 197 -} = s_separate (n .+ 1) FlowOut
                               & ns_flow_node (n .+ 1) FlowOut & s_l_comments

s_l__block_in_block n c {- 198 -} = wrapTokens BeginNode EndNode
                                  $ ( s_l__block_scalar n c / s_l__block_collection n c )
s_l__block_scalar n c   {- 199 -} = s_separate (n .+ 1) c
                                  & ( c_ns_properties (n .+ 1) c & s_separate (n .+ 1) c ?)
                                  & ( c_l__literal n / c_l__folded n )

s_l__block_collection n c {- 200 -} = ( s_separate (n .+ 1) c & c_ns_properties (n .+ 1) c & ( s_l_comments >?) ?)
                                    & s_l_comments
                                    & ( l__block_sequence (seq_spaces n c)
                                      / l__block_mapping n )
seq_spaces n c            {- 201 -} = case c of
                                           BlockOut -> n .- 1
                                           BlockIn  -> n
                                           _        -> error "unexpected node style pattern in seq_spaces"

-- 9.1.1 Document Prefix

l_document_prefix {- 202 -} = ( c_byte_order_mark ?) & ( nonEmpty l_comment *)

-- 9.1.2 Document Markers

c_directives_end  {- 203 -} = token DirectivesEnd [ '-', '-', '-' ]
c_document_end    {- 204 -} = token DocumentEnd [ '.', '.', '.' ]
l_document_suffix {- 205 -} = c_document_end & s_l_comments
c_forbidden       {- 206 -} = sol
                            & ( c_directives_end / c_document_end )
                            & ( b_char / s_white / eof )

-- 9.1.3 Explicit Documents

l_bare_document {- 207 -} = DeNode ^ s_l__block_node (-1) BlockIn
                            `forbidding` c_forbidden

-- 9.1.4 Explicit Documents

l_explicit_document {- 208 -} = ( c_directives_end & ( b_char / s_white / eof >?)) ! DeDoc
                              & ( ( l_bare_document
                                  / e_node & ( s_l_comments ?) & unparsed 0 ) `recovery` unparsed 0 )

-- 9.1.5 Directives Documents

l_directives_document {- 209 -} = ( l_directive +)
                                & l_explicit_document

-- 9.2 Streams:

l_any_document   {- 210 -} = wrapTokens BeginDocument EndDocument
                           $ DeDoc ^ ( l_directives_document
                                     / l_explicit_document
                                     / l_bare_document ) `recovery` unparsed 0

l_yaml_stream {- 211 -} = ( nonEmpty l_document_prefix *)
                        & ( eof / ( c_document_end & ( b_char / s_white / eof ) >?) / l_any_document )
                        & ( nonEmpty ( DeMore ^ ( ( l_document_suffix ! DeMore +) & ( nonEmpty l_document_prefix *) & ( eof / l_any_document )
                                                / ( nonEmpty l_document_prefix *) & DeDoc ^ ( wrapTokens BeginDocument EndDocument l_explicit_document ?) ) ) *)
