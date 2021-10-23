{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utilities to tokenize a C++ file.
module Lexer (lexer) where

import Locations
import PPToken

import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Void
import Lens.Micro
import Lens.Micro.Extras
import Numeric
import Text.Megaparsec

import qualified Data.Text as T
import qualified Data.Text.ICU.Char as I
import qualified Data.Text.ICU.Normalize as I

-- | Create a lexer for the given file, producing a stream of tokens.
-- Performs phases 1 through 3 of [lex.phases],
-- and automatically groups preprocessing directives in preparation of phase 4.
--
-- Lexes mostly according to the current C++23 working draft,
-- but does not currently support modules.
lexer :: Parsec Void T.Text [WithLoc PPToken]
lexer = fst <$> runStateT ppFile True

-- The lexer has two primary issues to deal with:
--  * There may be arbitrary line splices at any point
--  * We need to track line breaks to know when a preprocessing directive starts
--
-- To help solve these issues, we have the following rules for lexing primitives:
--  * Lexing a token always 'eats' all following whitespace, including comments and splices
--  * Whenever such whitespace has a newline, we update the parser state with that info
--  * We avoid using (fast) Megaparsec primitives except when necessary, so that...
--  * We use our own helpers that atomically take tokens until we reach a non-splice
--
-- See `splice`, `char`, `wspace`, and `lexeme`

-- | Parser with state determining whether we've started a new line yet
type Parser = StateT Bool (Parsec Void T.Text)

-- Helpers for handling lexemes

-- | Universal Character Names. Assumes we've already parsed a backslash.
--
-- Note we don't need to handle splices in UCNs, since this is UB according to [lex.phases]/2.
-- However, note in [lex.charset]/2 that we must do more checking
-- but only outside of character sequences; this is the purpose of the first parameter.
--
-- TODO: use ICU to determine valid code points?
ucn :: Bool -> Parser Char
ucn inChrSeq = do
  code_point <- readUCN 'u' 4 <|> readUCN 'U' 8 <?> "UCN"
  let character = chr code_point
  when (invalid code_point) . fail $ "Invalid code point: " ++ show code_point
  unless inChrSeq $ do
    when (is_control code_point) $ fail "UCN cannot represent control char"
    when (is_srcchar character) $ fail "UCN cannot represent member of source charset"
  pure character
 where
  hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
  readUCN c n = fst . head . readHex <$> (single c *> count n hexDigit)
  invalid p = p < 0 || p > 0x10FFFF || (0xD800 <= p && p <= 0xDFFF)
  is_control p = p < 0x1F || (0x7F <= p && p <= 0x9F)
  is_srcchar c =
    isAsciiLower c || isAsciiUpper c
      || c `elem` (" \t\v\f\n_{}[]#()<>%:;.?*+-/^&|~!=,'\"\\" :: [Char])

-- | Splice a single logical line. Assumes we've already parsed a backslash.
splice :: Parser ()
splice = void (takeWhileP Nothing (\x -> isSpace x && x /= '\n') *> single '\n') <?> "newline"

-- | Read one character from the input stream.
-- Handles splices and UCNs, so not appropriate in string literals.
-- Behaves as an atomic construct.
char :: Parser Char
char = try . label "character" $ do
  c <- anySingle
  if c == '\\'
    then ucn False <|> (splice >> char) <?> "character"
    else pure c

-- | Read one character from the input stream satisfying the given predicate.
maybeChar :: (Char -> Bool) -> Parser Char
maybeChar f = try $ mfilter f char

-- | Read the precise identifier
string :: T.Text -> Parser T.Text
string t = fast <|> slow
 where
  fast = chunk t
  slow = try . hidden . mfilter (== t) . fmap T.pack . count (T.length t) $ char

-- | Some (possibly spliced) whitespace.
-- Updates state if we passed a logical line terminator.
space1 :: Parser ()
space1 = some (maybeChar isSpace) >>= \x -> when ('\n' `elem` x) (put True)

-- | A (possibly spliced) line comment. Leaves the newline at the end.
skipLineComment :: Parser ()
skipLineComment = void $ string "//" >> some (maybeChar (/= '\n'))

-- | A (possibly spliced) block comment. Skips any interior newlines.
skipBlockComment :: Parser ()
skipBlockComment = void $ string "/*" *> manyTill char (string "*/")

-- | Skip all whitespace to the next token.
wspace :: Parser ()
wspace = skipMany $ choice [hidden space1, hidden skipLineComment, hidden skipBlockComment]

-- Lexemes

-- | Construct a lexeme with location information from a given parser.
-- Eats all whitespace after a token. @p@ must not succeed on empty input.
lexeme :: Parser a -> Parser (WithLoc a)
lexeme p = do
  l1 <- getSourcePos
  r <- p
  l2 <- getSourcePos
  put False
  wspace
  pure $ WithLoc (Location l1 l2) r

-- | Require we just saw a newline in the whitespace following the last token.
newline :: Parser ()
newline = eof <|> (get >>= guard <?> "newline")

-- | Require we did *not* see a newline in the most recent whitespace.
sameline :: Parser ()
sameline = get >>= guard . not

-- | Directive-introducing token @#@
directiveIntro :: Parser (WithLoc ())
directiveIntro = label "pp-directive" $ newline *> (symbol "#" <|> symbol "%:")

-- | A character suitable to begin an identifier
idStart :: Parser Char
idStart = maybeChar $ \c -> I.property I.XidStart c || c == '_'

-- | A character suitable to continue in an identifier
idContinue :: Parser Char
idContinue = maybeChar $ I.property I.XidContinue

-- | Ad-hoc keywords
keyword :: T.Text -> Parser (WithLoc T.Text)
keyword k = lexeme . try $ string k <* notFollowedBy idContinue

-- | Identifiers (including those that become keywords when tokenised)
identifier :: Parser (WithLoc T.Text)
identifier = lexeme identifier'

-- | Identifiers that aren't lexemes.
-- This is identical to @identifier@ except it does not parse succeeding whitespace.
-- As such using @getSourcePos@ will correctly get the end of this token.
identifier' :: Parser T.Text
identifier' = label "identifier" . try $ do
  t <- (:) <$> idStart <*> many idContinue <&> T.pack
  unless (I.isNormalized I.NFC t) $ fail "identifiers must be NFC normalized"
  pure t

-- | Operators
operator :: Parser (WithLoc Operator)
operator = label "operator" . choice $ operatorParsers

-- | Symbols
symbol :: T.Text -> Parser (WithLoc ())
symbol t = lexeme . label (T.unpack t) . void $ string t

-- | Encoding prefix for string or character literals
encodingPrefix :: Parser EncodingPrefix
encodingPrefix =
  string "u8" $> EncU8
    <|> string "u" $> EncLowerU
    <|> string "U" $> EncUpperU
    <|> string "L" $> EncL
    <|> pure EncNone
    <?> "encoding prefix"

-- | String literals; returns a tuple (encoding, string, suffix).
-- The string doesn't include surrounding quotes, and only interprets valid, unspliced UCNs.
stringLiteral :: Parser (WithLoc (EncodingPrefix, T.Text, Maybe T.Text))
stringLiteral = lexeme . label "string literal" $ rawLiteral <|> normalLiteral
 where
  dChar = noneOf (" ()\\\t\v\f\n" :: [Char]) <?> "raw string delimiter"
  rawLiteral = do
    e <- try $ encodingPrefix <* string "R\""
    d <- manyTill dChar (single '(') <&> T.pack
    cs <- manyTill anySingle (try $ single ')' *> chunk d *> single '"') <&> T.pack
    u <- optional identifier'
    pure (e, cs, u)
  normalLiteral = do
    e <- try $ encodingPrefix <* maybeChar (== '\"')
    cs <- manyTill cChar (single '\"') <&> T.pack . catMaybes
    u <- optional identifier'
    pure (e, cs, u)

-- | Character literals; returns a tuple (encoding, string, suffix).
-- The string doesn't include surrounding quotes, and only interprets valid, unspliced UCNs.
-- Supports multicharacter literals.
charLiteral :: Parser (WithLoc (EncodingPrefix, T.Text, Maybe T.Text))
charLiteral = lexeme . label "character literal" $ do
  e <- try $ encodingPrefix <* maybeChar (== '\'')
  cs <- manyTill cChar (single '\'') <&> T.pack . catMaybes
  u <- optional identifier'
  pure (e, cs, u)

-- | A single s-char or c-char, as used in (non-raw) character/string literals.
cChar :: Parser (Maybe Char)
cChar = escapeSequence <|> Just <$> anySingleBut '\n'

-- | An escape sequence in a string or character literal.
-- May return nothing if the escape was a splice.
escapeSequence :: Parser (Maybe Char)
escapeSequence = single '\\' >> escapes <?> "escape sequence"
 where
  escapes =
    choice
      [ Just <$> ucn True
      , Nothing <$ splice
      , Just <$> label "escape sequence" (try $ simpleEscape <|> octalEscape <|> hexEscape)
      ]
  simpleEscapeMap =
    [ ('\'', '\'')
    , ('"', '"')
    , ('?', '?')
    , ('\\', '\\')
    , ('a', '\a')
    , ('b', '\b')
    , ('f', '\f')
    , ('n', '\n')
    , ('r', '\r')
    , ('t', '\t')
    , ('v', '\v')
    ]
  simpleEscape = token (`lookup` simpleEscapeMap) mempty <?> "simple escape"
  octalEscape = label "octal escape" $ do
    o <- count' 1 3 (maybeChar isOctDigit <?> "octal digit") <&> fst . head . readOct
    when (o > ord maxBound) $ fail "octal escape character out of range"
    pure $ chr o
  hexEscape = label "hex escape" $ do
    o <- single 'x' *> some (maybeChar isHexDigit <?> "hex digit") <&> fst . head . readHex
    when (o > ord maxBound) $ fail "hex escape character out of range"
    pure $ chr o

--------------------------------------------------------------------------------
-- Actual parsing things

-- TODO: handle module stuff
ppFile :: Parser [WithLoc PPToken]
ppFile = wspace *> group <* eof

-- | Parse a group of PP tokens.
group :: Parser [WithLoc PPToken]
group = many ppToken

-- | A single PPToken.
-- Fails without consuming input for conditional inclusion,
-- to allow a possible upper-level to handle it.
ppToken :: Parser (WithLoc PPToken)
ppToken = do
  notFollowedBy (directiveIntro *> sameline *> endIfLike <?> "conditional")
  choice
    [ ppNumber
    , ifSection <&> value %~ Group
    , controlLine <&> value %~ Group
    , charLiteral <&> value %~ uncurry3 CharLiteral
    , stringLiteral <&> value %~ uncurry3 StringLiteral
    , hasIncludeExpr
    , operator <&> value %~ Operator
    , identifier <&> value %~ Identifier
    ]
 where
  endIfLike = choice $ map keyword ["elif", "elifdef", "elifndef", "else", "endif"]
  uncurry3 f (a, b, c) = f a b c
  hasIncludeExpr = try . hidden $ do
    k <- keyword "__has_include" <* sameline <* symbol "(" <* sameline
    h <- headerName <* sameline <&> HasIncludeExpr
    c <- symbol ")"
    pure $ WithLoc (k ^. location <> c ^. location) h

-- | A single ppnumber literal. This is not necessarily a /real/ numeric literal.
ppNumber :: Parser (WithLoc PPToken)
ppNumber = lexeme $ do
  c <- try $ digitT <|> T.append <$> period <*> digitT
  cs <- many $ choice [exponents, continue, digitSeparator, period]
  pure . Number $ mconcat (c : cs)
 where
  continue = T.pack <$> some idContinue
  digitSeparator = T.cons <$> maybeChar (== '\'') <*> (digitT <|> nonDigitT)
  exponents = try $ T.cons <$> maybeChar (`T.elem` "eEpP") <*> sign
  period = T.singleton <$> maybeChar (== '.')

  digitT = T.singleton <$> maybeChar isDigit
  nonDigitT = T.singleton <$> maybeChar (\c -> isAsciiLower c || isAsciiUpper c || c == '_')
  sign = T.singleton <$> maybeChar (\c -> c == '+' || c == '-') <?> "sign"

-- | Parse to the end of the line as a list of tokens, parsing at least one.
-- Maps the list of tokens using the given function to a result.
parseLineWith1 :: ([WithLoc PPToken] -> a) -> Parser (WithLoc a)
parseLineWith1 f = do
  ts <- someTill ppToken newline
  pure $ WithLoc (ts ^?! _last . location) (f ts)

-- | Parse to the end of the line as a list of tokens.
-- May parse no tokens, uses the location of the previous token if so.
-- Maps the list of tokens using the given function to a result.
parseLineWith :: ([WithLoc PPToken] -> a) -> WithLoc b -> Parser (WithLoc a)
parseLineWith f l = do
  ts <- manyTill ppToken newline
  pure $ WithLoc (fromMaybe (l ^. location) (ts ^? _last . location)) (f ts)

-- | Parse a header name as used in e.g. @#include <headername>@.
-- Only handles the forms @<foobar>@ and @"foobar"@;
-- should handle arbitrary pptoks separately.
headerName :: Parser (WithLoc PPInclude)
headerName = hchar <|> qchar <?> "header name"
 where
  common b e = single b *> someTill (maybeChar (/= '\n')) (maybeChar (== e))
  hchar = lexeme (IncludeHChar . T.pack <$> common '<' '>')
  qchar = lexeme (IncludeQChar . T.pack <$> common '"' '"')

-- | A control line, such as @#include@ or @#define@.
controlLine :: Parser (WithLoc PPGroup)
controlLine = directiveIntro >>= \i -> emptyD i <|> ctrl i
 where
  emptyD i = newline $> (i & value .~ EmptyGroup)
  ctrl i = choice opts <&> location . startLoc .~ (i ^. location . startLoc)
  opts = [include, define, undef, line, perror, pragma, unknown]

  include :: Parser (WithLoc PPGroup)
  include = do
    keyword "include" *> sameline
    h <- headerName <* newline <|> parseLineWith1 IncludePPTok
    pure $ h & value %~ Include

  define :: Parser (WithLoc PPGroup)
  define = do
    keyword "define" *> sameline <?> "macro identifier"
    -- any whitespace following the identifier here is semantic
    (b, i, e) <- (,,) <$> getSourcePos <*> identifier' <*> getSourcePos
    let i' = WithLoc (Location b e) i
    function i' <|> object i'
   where
    wsl = wspace *> sameline
    comma = maybeChar (== ',') *> wsl <?> "comma"
    object i = wspace *> parseLineWith (Define i) i <?> "object macro"
    function i = label "function macro" $ do
      () <- maybeChar (== '(') *> wsl
      ps <- sepBy (identifier <* sameline) (try $ comma *> notFollowedBy (symbol "..."))
      e <- isJust <$> optional (unless (null ps) comma *> symbol "..." *> sameline) <?> "variadic"
      c <- symbol ")" <?> "close paren"
      parseLineWith (DefineFn i ps e) c

  undef :: Parser (WithLoc PPGroup)
  undef = do
    _ <- keyword "undef"
    sameline <?> "macro identifier"
    identifier <* newline <&> value %~ Undef <?> "macro identifier"

  line :: Parser (WithLoc PPGroup)
  line = keyword "line" *> (sameline <?> "line number") *> parseLineWith1 Line

  perror :: Parser (WithLoc PPGroup)
  perror = keyword "error" >>= parseLineWith Error

  pragma :: Parser (WithLoc PPGroup)
  pragma = keyword "pragma" >>= parseLineWith Pragma

  unknown :: Parser (WithLoc PPGroup)
  unknown = parseLineWith1 UnknownGroup

-- | Recursively parse conditional preprocessing directives, like @#if@.
-- TODO: group with above control line, factor out #, properly fallback
ifSection :: Parser (WithLoc PPGroup)
ifSection = do
  i <- choice [ifGroup, ifDefGroup, ifNotDefGroup]
  es <- many $ choice [elifGroup, elifDefGroup, elifNotDefGroup]
  e <- optional elseGroup
  k <- directiveIntro *> sameline *> keyword "endif" <* newline
  i & location . endLoc .~ (k ^. location . endLoc) & value .~ IfSection i es e & pure
 where
  intro k = try $ directiveIntro <* sameline <* keyword k <&> view (location . startLoc)

  ifLike :: T.Text -> ([WithLoc PPToken] -> [WithLoc PPToken] -> a) -> Parser (WithLoc a)
  ifLike k f = do
    s <- intro k
    e <- someTill ppToken newline
    WithLoc (Location s $ e ^?! _last . location . endLoc) . f e <$> group

  ifDefLike :: T.Text -> (WithLoc T.Text -> [WithLoc PPToken] -> a) -> Parser (WithLoc a)
  ifDefLike k f = do
    s <- intro k <* sameline
    i <- identifier <* newline
    WithLoc (Location s $ i ^. location . endLoc) . f i <$> group

  ifGroup = ifLike "if" If
  ifDefGroup = ifDefLike "ifdef" IfDef
  ifNotDefGroup = ifDefLike "ifndef" IfNotDef

  elifGroup = ifLike "elif" Elif
  elifDefGroup = ifDefLike "elifdef" ElifDef
  elifNotDefGroup = ifDefLike "elifndef" ElifNotDef

  elseGroup = do
    (s, e) <- try $ (,) <$> (directiveIntro <* sameline) <*> (keyword "else" <* newline)
    WithLoc (s ^. location <> e ^. location) . Else <$> group

{- ORMOLU_DISABLE -}
-- | Full list of how to parse (non-preprocessing) operators.
-- Includes handling for alternative tokens (e.g. @and@).
operatorParsers :: [Parser (WithLoc Operator)]
operatorParsers =
  [ op "~" BitNot
  , op "##" PPJoin
  , op "#" PPStringify
  , op "}" RBrace
  , op "||" Or
  , op "|=" BitOrAssign
  , op "|" BitOr
  , op "{" LBrace
  , op "^=" BitXorAssign
  , op "^" BitXor
  , op "]" RBracket
  , op "[" LBracket
  , op "?" Conditional
  , op ">>=" BitRShiftAssign
  , op ">>" BitRShift
  , op ">=" GreaterEqual
  , op ">" Greater
  , op "==" Equal
  , op "=" Assign
  , op "<=>" ThreeWayEqual
  , op "<=" LessEqual
  , op "<<=" BitLShiftAssign
  , op "<<" BitLShift
  -- [lex.pptoken]/3.2
  , lookAhead (string "<::" *> maybeChar (\c -> c /= ':' && c /= '>')) *> op "<" Less
  , op "<:" LBrace
  , op "<%" LBracket
  , op "<" Less
  , op ";" Semicolon
  , op ":>" RBrace
  , op "::" Scope
  , op ":" Colon
  , op "/=" DivideAssign
  , op "/" Divide
  , op "..." Ellipsis
  , op ".*" MemberPtr
  , op "." Member
  , op "->*" ArrowPtr
  , op "->" Arrow
  , op "-=" MinusAssign
  , op "--" Decrement
  , op "-" Minus
  , op "," Comma
  , op "+=" PlusAssign
  , op "++" Increment
  , op "+" Plus
  , op "*=" MultiplyAssign
  , op "*" Multiply
  , op ")" RParen
  , op "(" LParen
  , op "&=" BitAndAssign
  , op "&&" And
  , op "&" BitAnd
  , op "%:%:" PPJoin
  , op "%:" PPStringify
  , op "%>" RBracket
  , op "%=" ModulusAssign
  , op "%" Modulus
  , op "!=" NotEqual
  , op "!" Not
  , kw "and_eq" BitAndAssign
  , kw "and" And
  , kw "or_eq" BitOrAssign
  , kw "or" Or
  , kw "xor_eq" BitXorAssign
  , kw "xor" BitXor
  , kw "not_eq" NotEqual
  , kw "not" Not
  , kw "bitand" BitAnd
  , kw "bitor" BitOr
  , kw "compl" BitNot
  ]
 where
  op t x = symbol t <&> value .~ x
  kw t x = keyword t <&> value .~ x
{- ORMOLU_ENABLE -}
