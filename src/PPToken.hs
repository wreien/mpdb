{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the data types for preprocessing tokens.
-- See [lex.pptoken].
module PPToken (
  PPToken (..),
  EncodingPrefix (..),
  PPGroup (..),
  PPInclude (..),
  PPIfGroup (..),
  PPElifGroup (..),
  PPElseGroup (..),
  Operator (..),
) where

import Locations

import Data.List (foldl')
import qualified Data.Text as T
import Lens.Micro
import Prettyprinter

-- | A preprocessing token.
--
-- /TODO/: separate `Group` from normal Tokens?
data PPToken
  = Identifier T.Text
  | Number T.Text
  | CharLiteral EncodingPrefix T.Text (Maybe T.Text)
  | StringLiteral EncodingPrefix T.Text (Maybe T.Text)
  | Operator Operator
  | HasIncludeExpr (WithLoc PPInclude)
  | Group PPGroup
  deriving (Eq, Ord, Show)

-- | A preprocessing directive (e.g. @#define@)
data PPGroup
  = -- | No directive
    EmptyGroup
  | -- | Represents a @#include@
    Include PPInclude
  | -- | Represents @#define ident replacement-tokens@
    Define (WithLoc T.Text) [WithLoc PPToken]
  | -- | Represents @#define ident(params, ...) replacement-tokens@
    DefineFn (WithLoc T.Text) [WithLoc T.Text] Bool [WithLoc PPToken]
  | -- | Represents @#undef ident@
    Undef T.Text
  | -- | @#line 123 \"filename\"@ (after preprocessing)
    Line [WithLoc PPToken]
  | -- | Represents @#error tokens@
    Error [WithLoc PPToken]
  | -- | Represents @#pragma tokens@
    Pragma [WithLoc PPToken]
  | -- | A collection of conditional statements
    IfSection (WithLoc PPIfGroup) [WithLoc PPElifGroup] (Maybe (WithLoc PPElseGroup))
  | -- | Some other group that's probably patched out
    UnknownGroup [WithLoc PPToken]
  deriving (Eq, Ord, Show)

-- | Different kinds of include directives.
data PPInclude
  = -- | @#include \<h-char-sequence\>@
    IncludeHChar T.Text
  | -- | @#include \"q-char-sequence\"@
    IncludeQChar T.Text
  | -- | @#include pp-tokens@
    IncludePPTok [WithLoc PPToken]
  deriving (Eq, Ord, Show)

-- | Character encoding, one of @u8@, @u@, @U@, @L@, or nothing.
data EncodingPrefix = EncNone | EncU8 | EncLowerU | EncUpperU | EncL
  deriving (Eq, Ord, Show)

-- | Tokens hidden behind a @#if@ directive
data PPIfGroup
  = If [WithLoc PPToken] [WithLoc PPToken]
  | IfDef (WithLoc T.Text) [WithLoc PPToken]
  | IfNotDef (WithLoc T.Text) [WithLoc PPToken]
  deriving (Eq, Ord, Show)

-- | Tokens hidden behind a @#elif@ directive
data PPElifGroup
  = Elif [WithLoc PPToken] [WithLoc PPToken]
  | ElifDef (WithLoc T.Text) [WithLoc PPToken]
  | ElifNotDef (WithLoc T.Text) [WithLoc PPToken]
  deriving (Eq, Ord, Show)

-- | Tokens hidden behind a @#else@ directive
newtype PPElseGroup = Else [WithLoc PPToken]
  deriving (Eq, Ord, Show)

-- | Different kinds of operators.
--
-- There is no @Dereference@ token, we just use `Multiply`.
data Operator
  = PPStringify
  | PPJoin
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | LParen
  | RParen
  | Semicolon
  | Comma
  | Conditional
  | Colon
  | Ellipsis
  | Scope
  | Member
  | MemberPtr
  | Arrow
  | ArrowPtr
  | Assign
  | Plus
  | PlusAssign
  | Minus
  | MinusAssign
  | Multiply
  | MultiplyAssign
  | Divide
  | DivideAssign
  | Modulus
  | ModulusAssign
  | BitNot
  | BitAnd
  | BitAndAssign
  | BitOr
  | BitOrAssign
  | BitXor
  | BitXorAssign
  | BitLShift
  | BitLShiftAssign
  | BitRShift
  | BitRShiftAssign
  | Increment
  | Decrement
  | Not
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | ThreeWayEqual
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty PPToken where
  pretty (Group g) = pretty g
  pretty (Identifier i) = pretty i
  pretty (Number n) = pretty n
  pretty (CharLiteral e c s) = pretty e <> squotes (pretty c) <> pretty s
  pretty (StringLiteral e c s) = pretty e <> dquotes (pretty c) <> pretty s
  pretty (Operator o) = pretty o
  pretty (HasIncludeExpr i) = "__has_include(" <> pretty i <> ")"

  -- whitespace separated, but PPGroups are on lines of their own
  prettyList [] = mempty
  prettyList [x] = pretty x
  prettyList (t : ts) = snd $ foldl' go (t, pretty t) ts
   where
    -- accumulator = (last token, current document)
    go :: (PPToken, Doc a) -> PPToken -> (PPToken, Doc a)
    go (_, d) t'@(Group _) = (t', d <> hardline <> pretty t')
    go (Group _, d) t' = (t', d <> hardline <> pretty t')
    go (_, d) t' = (t', d <+> pretty t')

instance Pretty EncodingPrefix where
  pretty EncNone = ""
  pretty EncU8 = "u8"
  pretty EncLowerU = "u"
  pretty EncUpperU = "U"
  pretty EncL = "L"

instance Pretty PPGroup where
  pretty EmptyGroup = "#"
  pretty (Include i) = "#include" <+> pretty i
  pretty (Define i ts) = "#define" <+> pretty i <+> pretty ts
  pretty (DefineFn f ps v ts) =
    let ps' = ps ^.. each . value
        ps'' = if v then ps' ++ ["..."] else ps'
        params = lparen <> (hsep . punctuate comma . map pretty) ps'' <> rparen
     in "#define" <+> pretty f <> params <+> pretty ts
  pretty (Undef i) = "#undef" <+> pretty i
  pretty (Line ts) = "#line" <+> pretty ts
  pretty (Error ts) = "#error" <+> pretty ts
  pretty (Pragma ts) = "#pragma" <+> pretty ts
  pretty (IfSection i es e) = pretty i <> pretty es <> pretty e
  pretty (UnknownGroup ts) = "#" <> pretty ts

instance Pretty PPInclude where
  pretty (IncludeHChar h) = angles (pretty h)
  pretty (IncludeQChar q) = dquotes (pretty q)
  pretty (IncludePPTok ts) = pretty ts

instance Pretty PPIfGroup where
  pretty =
    nest 2 . \case
      (If ts g) -> "#if" <+> pretty ts <> hardline <> pretty g
      (IfDef i g) -> "#ifdef" <+> pretty i <> hardline <> pretty g
      (IfNotDef i g) -> "#ifndef" <+> pretty i <> hardline <> pretty g

instance Pretty PPElifGroup where
  pretty =
    nest 2 . \case
      (Elif ts g) -> "#elif" <+> pretty ts <> hardline <> pretty g
      (ElifDef i g) -> "#elifdef" <+> pretty i <> hardline <> pretty g
      (ElifNotDef i g) -> "#elifndef" <+> pretty i <> hardline <> pretty g

instance Pretty PPElseGroup where
  pretty (Else g) = nest 2 $ "#else" <> hardline <> pretty g

instance Pretty Operator where
  pretty = \case
    PPStringify -> "#"
    PPJoin -> "##"
    LBrace -> "{"
    RBrace -> "}"
    LBracket -> "["
    RBracket -> "]"
    LParen -> "("
    RParen -> ")"
    Semicolon -> ";"
    Comma -> ","
    Conditional -> "?"
    Colon -> ":"
    Ellipsis -> "..."
    Scope -> "::"
    Member -> "."
    MemberPtr -> ".*"
    Arrow -> "->"
    ArrowPtr -> "->*"
    Assign -> "="
    Plus -> "+"
    PlusAssign -> "+="
    Minus -> "-"
    MinusAssign -> "-="
    Multiply -> "*"
    MultiplyAssign -> "*="
    Divide -> "/"
    DivideAssign -> "/="
    Modulus -> "%"
    ModulusAssign -> "%="
    BitNot -> "~"
    BitAnd -> "&"
    BitAndAssign -> "&="
    BitOr -> "|"
    BitOrAssign -> "|="
    BitXor -> "^"
    BitXorAssign -> "^="
    BitLShift -> "<<"
    BitLShiftAssign -> "<<="
    BitRShift -> ">>"
    BitRShiftAssign -> ">>="
    Increment -> "++"
    Decrement -> "--"
    Not -> "!"
    And -> "&&"
    Or -> "||"
    Equal -> "=="
    NotEqual -> "!="
    Less -> "<"
    Greater -> ">"
    LessEqual -> "<="
    GreaterEqual -> ">="
    ThreeWayEqual -> "<=>"
