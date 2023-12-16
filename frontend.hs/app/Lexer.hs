{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Token (..),
    SpannedTok,
    lexTokens,
  )
where

import Control.Applicative ((<|>))
import Data.Char (Char, isAlphaNum)
import Data.Data (Data)
import Data.Functor (($>))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set, toList)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Syntax (Spanned (..))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error
  ( ErrorFancy (ErrorCustom, ErrorFail, ErrorIndentation),
    ErrorItem (EndOfInput, Label, Tokens),
    ParseError (FancyError),
    ParseErrorBundle,
    bundleErrors,
  )

type Parser = M.Parsec Void Text

data Token
  = TInt Int64
  | TDouble Double
  | TStr Text
  | TChar Char
  | TIdent Text
  | TBool Bool
  | -- Keywords
    TLet
  | TIn
  | TIf
  | TThen
  | TWhile
  | TFor
  | TElse
  | TTrue
  | TFalse
  | TFun
  | TReturn
  | TMake
  | TCast
  | TSahl
  | TBreak
  | TContinue
  | TExtern
  | TTypedef
  | TIs
  | TMatch
  | TConst
  | -- Symbols
    TTilde
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TPercent
  | TCaret
  | TAmpersand
  | TPipe
  | TAssign
  | TEqual
  | TNotEqual
  | TLessThan
  | TLessThanOrEqual
  | TGreaterThan
  | TGreaterThanOrEqual
  | TLeftShift
  | TRightShift
  | TAnd
  | TOr
  | TNot
  | TDot
  | TComma
  | TColon
  | TSemicolon
  | TLeftParen
  | TRightParen
  | TLeftBracket
  | TRightBracket
  | TLeftBrace
  | TRightBrace
  | TLeftArrow
  | TRightArrow
  | TRange
  | TRef
  deriving (Show, Eq, Ord, Typeable, Data)

space :: Parser ()
space = do L.space MC.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

keywords :: [Text]
keywords =
  [ "let",
    "in",
    "if",
    "then",
    "else",
    "true",
    "false",
    "fun",
    "return",
    "while",
    "make",
    "cast",
    "for",
    "sahl",
    "break",
    "continue",
    "ref",
    "extern",
    "type",
    "is",
    "match",
    "const"
  ]

identifier :: Parser SpannedTok
identifier = withOffset $ do
  name <- lexeme $ M.takeWhile1P Nothing isAlphaNum
  if name `elem` keywords
    then return $ case name of
      "let" -> TLet
      "in" -> TIn
      "if" -> TIf
      "then" -> TThen
      "else" -> TElse
      "true" -> TTrue
      "false" -> TFalse
      "fun" -> TFun
      "return" -> TReturn
      "while" -> TWhile
      "make" -> TMake
      "cast" -> TCast
      "for" -> TFor
      "sahl" -> TSahl
      "break" -> TBreak
      "continue" -> TContinue
      "ref" -> TRef
      "extern" -> TExtern
      "type" -> TTypedef
      "is" -> TIs
      "match" -> TMatch
      "const" -> TConst
    else return $ TIdent name

withOffset :: Parser a -> Parser (Spanned a)
withOffset p = do
  start <- M.getOffset
  x <- p
  end <- M.getOffset
  return ((start, end), x)

type SpannedTok = Spanned Token

bool :: Parser SpannedTok
bool =
  withOffset $
    TBool
      <$> ( (MC.string "true" $> True)
              <|> (MC.string "false" $> False)
          )

nat :: Parser SpannedTok
nat = withOffset (TInt <$> L.decimal)

hex :: Parser SpannedTok
hex = withOffset (TInt <$> L.hexadecimal)

oct :: Parser SpannedTok
oct = withOffset (TInt <$> L.octal)

bin :: Parser SpannedTok
bin = withOffset (TInt <$> L.binary)

integer :: Parser SpannedTok
integer = M.choice [hex, oct, bin, nat]

double :: Parser SpannedTok
double = withOffset (TDouble <$> L.float)

str :: Parser SpannedTok
str = withOffset (TStr . pack <$> (MC.char '"' *> L.charLiteral `M.manyTill` (MC.char '"' <* space)))

char :: Parser SpannedTok
char = withOffset (TChar <$> (MC.char '\'' *> L.charLiteral <* MC.char '\''))

symbol :: Parser SpannedTok
symbol = withOffset $ M.choice $ map (\(s, t) -> t <$ MC.string s) symbols
  where
    symbols =
      [ ("~", TTilde),
        ("+", TPlus),
        ("-", TMinus),
        ("*", TStar),
        ("/", TSlash),
        ("%", TPercent),
        ("^", TCaret),
        ("&", TAmpersand),
        ("|", TPipe),
        ("=", TAssign),
        ("==", TEqual),
        ("!=", TNotEqual),
        ("<", TLessThan),
        ("<=", TLessThanOrEqual),
        (">", TGreaterThan),
        (">=", TGreaterThanOrEqual),
        ("<<", TLeftShift),
        (">>", TRightShift),
        ("&&", TAnd),
        ("||", TOr),
        ("!", TNot),
        (".", TDot),
        (",", TComma),
        (":", TColon),
        (";", TSemicolon),
        ("(", TLeftParen),
        (")", TRightParen),
        ("[", TLeftBracket),
        ("]", TRightBracket),
        ("{", TLeftBrace),
        ("}", TRightBrace),
        ("->", TRightArrow),
        ("<-", TLeftArrow),
        ("..", TRange)
      ]

token :: Parser SpannedTok
token = M.choice [bool, integer, double, str, char, symbol, identifier]

tokens :: Parser [SpannedTok]
tokens = M.many (space *> token)

lexTokens ::
  String ->
  Text ->
  Either (ParseErrorBundle Text Void) [SpannedTok]
lexTokens = M.parse tokens
