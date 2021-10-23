{-# LANGUAGE OverloadedStrings #-}

{- ORMOLU_DISABLE -}

module LexerSpec (spec) where

import qualified Data.Text as T
import Lexer (lexer)
import Locations
import PPToken
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (SourcePos (..), mkPos, parse)

spec :: Spec
spec = describe "lexer" $ do
  whitespace
  identifiers
  numbers
  characters
  strings
  rawstrings
  operators
  controlLine
  ifSections

infixl 3 @:
(@:) :: a -> Location -> WithLoc a
(@:) = flip WithLoc

infixl 4 -|
(-|) :: SourcePos -> SourcePos -> Location
(-|) = Location

infixl 5 .^
(.^) :: Int -> Int -> SourcePos
l .^ c = SourcePos "" (mkPos l) (mkPos c)

infixl 4 ^|
(^|) :: Int -> Int -> Location
x ^| y = 1 .^ x -| 1 .^ y

infixr 1 ==>
(==>) :: T.Text -> [WithLoc PPToken] -> Expectation
t ==> v = parse lexer "" t `shouldParse` v

-- TODO: assert error information is good too
-- the issue is... currently error messages are awful ;)
shouldNotLex :: T.Text -> Expectation
shouldNotLex t = parse lexer "" `shouldFailOn` t

whitespace :: Spec
whitespace = describe "whitespace" $ do
  it "handles leading and trailing whitespace" $ do
    " \t\n\f" ==> []
    "  x" ==> [Identifier "x" @: 3 ^| 4]
    "y\n\n\v" ==> [Identifier "y" @: 1 ^| 2]
  it "skips line comments" $ do
    "a//b" ==> [Identifier "a" @: 1 ^| 2]
    "a/\\\n/b" ==> [Identifier "a" @: 1 ^| 2]
    "a//b\nc" ==>
      [ Identifier "a" @: 1 .^ 1 -| 1 .^ 2
      , Identifier "c" @: 2 .^ 1 -| 2 .^ 2
      ]
  it "skips block comments" $ do
    "/*a*/b" ==> [Identifier "b" @: 6 ^| 7]
    "x/*\"\ny*/z" ==>
      [ Identifier "x" @: 1 .^ 1 -| 1 .^ 2
      , Identifier "z" @: 2 .^ 4 -| 2 .^ 5
      ]
    "/\\  \n*/**/*/" ==>
      [ Operator Multiply @: 2 .^ 6 -| 2 .^ 7
      , Operator Divide @: 2 .^ 7 -| 2 .^ 8
      ]
  it "fails on unterminated block comments" $ do
    shouldNotLex "/* * /"
  it "ignores newlines within block comments" $ do
    "#/*\n*/define x" ==> [Group (Define ("x" @: 2 .^ 10 -| 2 .^ 11) []) @: 1 .^ 1 -| 2 .^ 11]

identifiers :: Spec
identifiers = describe "identifiers" $ do
  it "parses simple identifiers" $ do
    "a" ==> [Identifier "a" @: 1 ^| 2]
    "_" ==> [Identifier "_" @: 1 ^| 2]
    "ሴ" ==> [Identifier "ሴ" @: 1 ^| 2]
    "x0" ==> [Identifier "x0" @: 1 ^| 3]
  it "doesn't get confused with literal encodings" $ do
    "u" ==> [Identifier "u" @: 1 ^| 2]
    "UR" ==> [Identifier "UR" @: 1 ^| 3]
    "L R\"()\"" ==> [Identifier "L" @: 1 ^| 2, StringLiteral EncNone "" Nothing @: 3 ^| 8]
  it "doesn't get confused with alternative tokens" $ do
    "ands" ==> [Identifier "ands" @: 1 ^| 5]
    "xor_" ==> [Identifier "xor_" @: 1 ^| 5]
  -- TODO: test normalisation somehow? :s

numbers :: Spec
numbers = describe "numbers" $ do
  it "parses normal numbers" $ do
    "123" ==> [Number "123" @: 1 ^| 4]
    "1.5" ==> [Number "1.5" @: 1 ^| 4]
    ".62" ==> [Number ".62" @: 1 ^| 4]
    "75." ==> [Number "75." @: 1 ^| 4]
  it "parses numbers in other bases" $ do
    "0b1010" ==> [Number "0b1010" @: 1 ^| 7]
    "0xaF0" ==> [Number "0xaF0" @: 1 ^| 6]
  it "parses numbers with digit separators" $ do
    "123'456" ==> [Number "123'456" @: 1 ^| 8]
    "9'9'9" ==> [Number "9'9'9" @: 1 ^| 6]
    "9'f'9" ==> [Number "9'f'9" @: 1 ^| 6]
    shouldNotLex "'123"
    shouldNotLex "456'"
  it "parses numbers with exponents" $ do
    "1e-2" ==> [Number "1e-2" @: 1 ^| 5]
    "1E+2" ==> [Number "1E+2" @: 1 ^| 5]
    "5.P+32" ==> [Number "5.P+32" @: 1 ^| 7]
    "1f+2" ==> [Number "1f" @: 1 ^| 3, Operator Plus @: 3 ^| 4, Number "2" @: 4 ^| 5]
  it "parses numbers with suffixes" $ do
    "1_hello" ==> [Number "1_hello" @: 1 ^| 8]
    "3ULL" ==> [Number "3ULL" @: 1 ^| 5]
  it "handles line splices in numbers" $ do
    "3\\\n4" ==> [Number "34" @: 1 .^ 1 -| 2 .^ 2]

characters :: Spec
characters = describe "character literals" $ do
  it "parses simple character literals" $ do
    "'x'" ==> [CharLiteral EncNone "x" Nothing @: 1 ^| 4]
    "'ሴ'" ==> [CharLiteral EncNone "ሴ" Nothing @: 1 ^| 4]
    "'\"'" ==> [CharLiteral EncNone "\"" Nothing @: 1 ^| 4]
  it "parses escape sequences" $ do
    "'\\''" ==> [CharLiteral EncNone "'" Nothing @: 1 ^| 5]
    "'\\012'" ==> [CharLiteral EncNone "\010" Nothing @: 1 ^| 7]
    "'\\xa3'" ==> [CharLiteral EncNone "\xa3" Nothing @: 1 ^| 7]
    "'\\  \nx'" ==> [CharLiteral EncNone "x" Nothing @: 1 .^ 1 -| 2 .^ 3]
    "'\\u1234'" ==> [CharLiteral EncNone "ሴ" Nothing @: 1 ^| 9]
  it "parses char literals with encodings" $ do
    "u'U'" ==> [CharLiteral EncLowerU "U" Nothing @: 1 ^| 5]
    "U'u'" ==> [CharLiteral EncUpperU "u" Nothing @: 1 ^| 5]
    "u8'8'" ==> [CharLiteral EncU8 "8" Nothing @: 1 ^| 6]
    "L'L'" ==> [CharLiteral EncL "L" Nothing @: 1 ^| 5]
  it "parses multicharacter literals" $ do
    "'1234'" ==> [CharLiteral EncNone "1234" Nothing @: 1 ^| 7]
  it "parses char literals with suffixes" $ do
    "'x'_hello" ==> [CharLiteral EncNone "x" (Just "_hello") @: 1 ^| 10]
    "U'U'U" ==> [CharLiteral EncUpperU "U" (Just "U") @: 1 ^| 6]
  it "handles unterminated character literals" $ do
    shouldNotLex "'x"
    shouldNotLex "'x\n'"

strings :: Spec
strings = describe "string literals" $ do
  it "parses simple string literals" $ do
    "\"xyz\"" ==> [StringLiteral EncNone "xyz" Nothing @: 1 ^| 6]
    "\"ሴ\"" ==> [StringLiteral EncNone "ሴ" Nothing @: 1 ^| 4]
    "\"\"" ==> [StringLiteral EncNone "" Nothing @: 1 ^| 3]
  it "parses escape sequences" $ do
    "\"\\\"\"" ==> [StringLiteral EncNone "\"" Nothing @: 1 ^| 5]
    "\"\\012\"" ==> [StringLiteral EncNone "\010" Nothing @: 1 ^| 7]
    "\"\\xa3\"" ==> [StringLiteral EncNone "\xa3" Nothing @: 1 ^| 7]
    "\"\\  \nx\"" ==> [StringLiteral EncNone "x" Nothing @: 1 .^ 1 -| 2 .^ 3]
    "\"\\u1234\"" ==> [StringLiteral EncNone "ሴ" Nothing @: 1 ^| 9]
  it "parses string literals with encodings" $ do
    "u\"U\"" ==> [StringLiteral EncLowerU "U" Nothing @: 1 ^| 5]
    "U\"u\"" ==> [StringLiteral EncUpperU "u" Nothing @: 1 ^| 5]
    "u8\"u8\"" ==> [StringLiteral EncU8 "u8" Nothing @: 1 ^| 7]
    "L\"L\"" ==> [StringLiteral EncL "L" Nothing @: 1 ^| 5]
  it "parses string literals with suffixes" $ do
    "\"x\"_hello" ==> [StringLiteral EncNone "x" (Just "_hello") @: 1 ^| 10]
    "\"string\"s" ==> [StringLiteral EncNone "string" (Just "s") @: 1 ^| 10]
    "U\"U\"U" ==> [StringLiteral EncUpperU "U" (Just "U") @: 1 ^| 6]
  it "handles unterminated string literals" $ do
    shouldNotLex "\"x"
    shouldNotLex "\"x\n\""

rawstrings :: Spec
rawstrings = describe "raw string literals" $ do
  it "parses simple raw string literals" $ do
    "R\"(xyz)\"" ==> [StringLiteral EncNone "xyz" Nothing @: 1 ^| 9]
    "R\"bf(ሴ)bf\"" ==> [StringLiteral EncNone "ሴ" Nothing @: 1 ^| 11]
    "R\"a()a\"" ==> [StringLiteral EncNone "" Nothing @: 1 ^| 8]
    "R\"x(x)\")x\"" ==> [StringLiteral EncNone "x)\"" Nothing @: 1 ^| 11]
  it "does not parse escape sequences" $ do
    "R\"xx(\\\")xx\"" ==> [StringLiteral EncNone "\\\"" Nothing @: 1 ^| 12]
    "R\"(\\012)\"" ==> [StringLiteral EncNone "\\012" Nothing @: 1 ^| 10]
    "R\"2(\\xa3)2\"" ==> [StringLiteral EncNone "\\xa3" Nothing @: 1 ^| 12]
    "R\"\"(\\  \nx)\"\"" ==> [StringLiteral EncNone "\\  \nx" Nothing @: 1 .^ 1 -| 2 .^ 5]
    "R\"(\\u1234)\"" ==> [StringLiteral EncNone "\\u1234" Nothing @: 1 ^| 12]
  it "parses raw string literals with encodings" $ do
    "uR\"(U)\"" ==> [StringLiteral EncLowerU "U" Nothing @: 1 ^| 8]
    "UR\"U(u)U\"" ==> [StringLiteral EncUpperU "u" Nothing @: 1 ^| 10]
    "u8R\"(u8)\"" ==> [StringLiteral EncU8 "u8" Nothing @: 1 ^| 10]
    "LR\"(L)\"" ==> [StringLiteral EncL "L" Nothing @: 1 ^| 8]
  it "parses raw string literals with suffixes" $ do
    "R\"(x)\"_hello" ==> [StringLiteral EncNone "x" (Just "_hello") @: 1 ^| 13]
    "R\"s(string)s\"s" ==> [StringLiteral EncNone "string" (Just "s") @: 1 ^| 15]
    "UR\"U(U)U\"U" ==> [StringLiteral EncUpperU "U" (Just "U") @: 1 ^| 11]
  it "handles unterminated string literals" $ do
    shouldNotLex "R\"(x"
    shouldNotLex "R\"ab(x)ac\""
  it "handles illegal delimiter sequences" $ do
    shouldNotLex "R\" (x) \""
    shouldNotLex "R\"\\(x)\\\""

operators :: Spec
operators = describe "operators" $ do
  it "handles simple operators" $ do
    "+" ==> [Operator Plus @: 1 ^| 2]
    "++" ==> [Operator Increment @: 1 ^| 3]
    "<<=" ==> [Operator BitLShiftAssign @: 1 ^| 4]
  it "handles alternative tokens" $ do
    "and" ==> [Operator And @: 1 ^| 4]
    "xor_eq" ==> [Operator BitXorAssign @: 1 ^| 7]
    "bitor" ==> [Operator BitOr @: 1 ^| 6]
    "%>" ==> [Operator RBracket @: 1 ^| 3]
  it "handles splices within operators" $ do
    "-\\ \n-" ==> [Operator Decrement @: 1 .^ 1 -| 2 .^ 2]
  it "performs maximal munch" $ do
    "+++++" ==>
      [ Operator Increment @: 1 ^| 3
      , Operator Increment @: 3 ^| 5
      , Operator Plus @: 5 ^| 6
      ]
  it "contextually parses preprocessing operators" $ do
    "# define a(b) #b" ==>
      [ Group (
          DefineFn ("a" @: 10 ^| 11) ["b" @: 12 ^| 13] False
            [ Operator PPStringify @: 15 ^| 16
            , Identifier "b" @: 16 ^| 17
            ]
        ) @: 1 ^| 17
      ]
    "%:define a(b) b%:%:b\n" ==>
      [ Group (
          DefineFn ("a" @: 10 ^| 11) ["b" @: 12 ^| 13] False
            [ Identifier "b" @: 15 ^| 16
            , Operator PPJoin @: 16 ^| 20
            , Identifier "b" @: 20 ^| 21
            ]
        ) @: 1 ^| 21
      ]

controlLine :: Spec
controlLine = describe "control line" $ do
  it "parses empty groups" $ do
    "#" ==> [Group EmptyGroup @: 1 ^| 2]
    "\n# // xyz\n" ==> [Group EmptyGroup @: 2 .^ 1 -| 2 .^ 2]
  it "parses #include" $ do
    "#include <iostream>" ==> [Group (Include $ IncludeHChar "iostream") @: 1 ^| 20]
    "# include < <<\"\t>" ==> [Group (Include $ IncludeHChar " <<\"\t") @: 1 ^| 18]
    "#include \"std<o.h\"" ==> [Group (Include $ IncludeQChar "std<o.h") @: 1 ^| 19]
    "#include x" ==> [Group (Include $ IncludePPTok [Identifier "x" @: 10 ^| 11]) @: 1 ^| 11]
  it "parses object-like #define" $ do
    "# define x" ==> [Group (Define ("x" @: 10 ^| 11) []) @: 1 ^| 11]
    "#define x y" ==> [Group (Define ("x" @: 9 ^| 10) [Identifier "y" @: 11 ^| 12]) @: 1 ^| 12]
    "#define x (z)" ==>
      [ Group (
          Define ("x" @: 9 ^| 10)
            [ Operator LParen @: 11 ^| 12
            , Identifier "z" @: 12 ^| 13
            , Operator RParen @: 13 ^| 14
            ]
        ) @: 1 ^| 14
      ]
    "#def\\\nine x/**/(\n)" ==>
      [ Group (
          Define ("x" @: 2 .^ 5 -| 2 .^ 6) [ Operator LParen @: 2 .^ 10 -| 2 .^ 11 ]
        ) @: 1 .^ 1 -| 2 .^ 11
      , Operator RParen @: 3 .^ 1 -| 3 .^ 2
      ]
  it "parses function-like #define" $ do
    "# def\\   \nine x()" ==> 
      [ Group (DefineFn ("x" @: 2 .^ 5 -| 2 .^ 6) [] False []) @: 1 .^ 1 -| 2 .^ 8 ]
    "#define x(a)\nb" ==> 
      [ Group (DefineFn ("x" @: 9 ^| 10) ["a" @: 11 ^| 12] False []) @: 1 ^| 13
      , Identifier "b" @: 2 .^ 1 -| 2 .^ 2
      ]
    "#define x(...)" ==> 
      [ Group (DefineFn ("x" @: 9 ^| 10) [] True []) @: 1 ^| 15 ]
    "#define x(a, ..\\\n.)" ==> 
      [ Group (DefineFn ("x" @: 9 ^| 10) ["a" @: 11 ^| 12] True []) @: 1 .^ 1 -| 2 .^ 3 ]
    "#define a(...) 2\n3" ==>
      [ Group (DefineFn ("a" @: 9 ^| 10) [] True [Number "2" @: 16 ^| 17]) @: 1 ^| 17
      , Number "3" @: 2 .^ 1 -| 2 .^ 2
      ]
    shouldNotLex "#define x(a,)"
    shouldNotLex "#define x(,...)"
    shouldNotLex "#define x(..., a)"
    shouldNotLex "#define x(\n)"
    shouldNotLex "#define x(0)"
  it "parses #undef" $ do
    "# undef x" ==> [Group (Undef "x") @: 1 ^| 10]
    "# u\\\nndef \\u1234" ==> [Group (Undef "ሴ") @: 1 .^ 1 -| 2 .^ 12]
    shouldNotLex "#undef\nx"
    shouldNotLex "#undef x y"
  it "parses #line" $ do
    "# li\\ \nne 12\\\n3" ==> 
      [ Group ( Line [ Number "123" @: 2 .^ 4 -| 3 .^ 2 ]) @: 1 .^ 1 -| 3 .^ 2 ]
    "#line 1 \"\" X" ==> 
      [ Group ( Line
          [ Number "1" @: 7 ^| 8
          , StringLiteral EncNone "" Nothing @: 9 ^| 11
          , Identifier "X" @: 12 ^| 13
          ]
        ) @: 1 ^| 13
      ]
    shouldNotLex "#line\n123"
  it "parses #error" $ do
    "#   error" ==> [Group (Error []) @: 1 ^| 10]
    "#err\\\nor e" ==> [Group (Error [Identifier "e" @: 2 .^ 4 -| 2 .^ 5]) @: 1 .^ 1 -| 2 .^ 5]
  it "parses #pragma" $ do
    "#  pragma" ==> [Group (Pragma []) @: 1 ^| 10]
    "#prag\\\nma p" ==> [Group (Pragma [Identifier "p" @: 2 .^ 4 -| 2 .^ 5]) @: 1 .^ 1 -| 2 .^ 5]
  it "parses other (unknown) groups" $ do
    "#1 x" ==> [Group (UnknownGroup [Number "1" @: 2 ^| 3, Identifier "x" @: 4 ^| 5]) @: 1 ^| 5]
  it "specially handles __has_include" $ do
    "__has_include(<foobar>)" ==> 
      [ HasIncludeExpr (IncludeHChar "foobar" @: 15 ^| 23) @: 1 ^| 24 ]
    "__has_include(xyz)" ==>
      [ Identifier "__has_include" @: 1 ^| 14
      , Operator LParen @: 14 ^| 15
      , Identifier "xyz" @: 15 ^| 18
      , Operator RParen @: 18 ^| 19
      ]

ifSections :: Spec
ifSections = describe "conditionals" $ do
  it "parses simple if directives" $ do
    "#if a\nb\n#endif" ==>
      [ Group
        ( IfSection 
          ( If
              [Identifier "a" @: 1 .^ 5 -| 1 .^ 6] 
              [Identifier "b" @: 2 .^ 1 -| 2 .^ 2] 
              @: 1 .^ 1 -| 1 .^ 6
          ) 
          [] 
          Nothing
        ) @: 1 .^ 1 -| 3 .^ 7
      ]
    "#if\\ \ndef a\nb 1\n#end\\\t\nif" ==>
      [ Group
        ( IfSection 
          ( IfDef
            ("a" @: 2 .^ 5 -| 2 .^ 6)
            [Identifier "b" @: 3 .^ 1 -| 3 .^ 2, Number "1" @: 3 .^ 3 -| 3 .^ 4]
            @: 1 .^ 1 -| 2 .^ 6
          ) 
          [] 
          Nothing
        ) @: 1 .^ 1 -| 5 .^ 3
      ]
  it "parses ifs with else directives" $ do
    "#ifdef x\n#elif 1\n2\n#else\n#endif" ==>
      [ Group
        ( IfSection
          ( IfDef ("x" @: 1 .^ 8 -| 1 .^ 9) [] @: 1 .^ 1 -| 1 .^ 9 )
          [ Elif
            [Number "1" @: 2 .^ 7 -| 2 .^ 8]
            [Number "2" @: 3 .^ 1 -| 3 .^ 2]
            @: 2 .^ 1 -| 2 .^ 8
          ]
          ( Just (Else [] @: 4 .^ 1 -| 4 .^ 6 ) )
        ) @: 1 .^ 1 -| 5 .^ 7
      ]
  it "parses nested ifs" $ do
    "#ifdef a\n#ifdef b\nc\n#endif\n#endif" ==>
      [ Group
        ( IfSection
          ( IfDef
            ("a" @: 1 .^ 8 -| 1 .^ 9)
            [ Group 
              ( IfSection
                ( IfDef 
                  ("b" @: 2 .^ 8 -| 2 .^ 9) 
                  [Identifier "c" @: 3 .^ 1 -| 3 .^ 2]
                  @: 2 .^ 1 -| 2 .^ 9 
                )
                []
                Nothing
              ) @: 2 .^ 1 -| 4 .^ 7
            ]
            @: 1 .^ 1 -| 1 .^ 9
          )
          []
          Nothing
        ) @: 1 .^ 1 -| 5 .^ 7
      ]
  it "handles unmatched conditionals" $ do
    shouldNotLex "#if 1"
    shouldNotLex "#else"
    shouldNotLex "#if a\n#if b\n#endif"
    shouldNotLex "#if\n#endif"
    shouldNotLex "#ifdef\n#endif"
    shouldNotLex "#if 1\n#else\n#else\n#endif"
    shouldNotLex "#if 1\n#else\n#elif 2\n#endif"
