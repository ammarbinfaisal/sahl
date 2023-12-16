{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative (Applicative (liftA2), (<**>), (<|>))
import Data.Bifunctor (second)
import Data.Char (Char, isAlphaNum)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Set (Set, toList)
import Data.Text (Text, cons, intercalate, pack, unpack)
import Data.Typeable (Typeable (..), typeOf)
import Data.Void (Void)
import Lexer (SpannedTok, Token (TIdent))
import qualified Lexer as L
import qualified Syntax as S
import qualified Syntax as Sy
import Text.Megaparsec (MonadParsec (token))
import qualified Text.Megaparsec as M
import Text.Megaparsec.Error
  ( ErrorFancy (ErrorCustom, ErrorFail, ErrorIndentation),
    ErrorItem (EndOfInput, Label, Tokens),
    ParseError (FancyError),
    ParseErrorBundle,
    bundleErrors,
  )

type Parser = M.Parsec Void [SpannedTok]

type SpannedExpr = Sy.Spanned Sy.Expr

satisfy :: (Token -> Bool) -> Parser SpannedTok
satisfy f = M.satisfy (\(p, tok) -> f tok)

consume :: Token -> Parser SpannedTok
consume tok = satisfy (== tok)

delimited :: Token -> Token -> Parser a -> Parser a
delimited open close p = do
  M.try $ do
    consume open
    xs <- p
    consume close
    return xs

simpleType :: Parser Sy.Type
simpleType = do
  -- on getting a type of types[i], return typess[i]
  M.choice $ zipWith (<$) typess (M.try . consume <$> types)
  where
    typess = [Sy.TInt, Sy.TDouble, Sy.TStr, Sy.TChar, Sy.TBool, Sy.TStr]
    types = TIdent <$> ["int", "double", "str", "char", "bool", "string"]

listty :: Parser Sy.Type
listty = M.try $ delimited L.TLeftBracket L.TRightBracket typee <&> Sy.TList

-- chan<type>
chanty :: Parser Sy.Type
chanty = M.try $ consume (L.TIdent "chan") *> delimited L.TLessThan L.TGreaterThan typee <&> Sy.TChan

-- (type, type, type)
tupleTy :: Parser Sy.Type
tupleTy = M.try $ delimited L.TLeftParen L.TRightParen (typee `M.sepBy` consume L.TComma) <&> Sy.TTuple

-- map<type, type>
mapty :: Parser Sy.Type
mapty =
  M.try $
    consume
      (L.TIdent "map")
      *> delimited L.TLessThan L.TGreaterThan ((,) <$> typee <*> typee)
      <&> uncurry Sy.TMap

typee :: Parser Sy.Type
typee = M.choice [simpleType, listty, chanty, tupleTy, mapty]

withSpan :: Parser a -> Parser (Sy.Spanned a)
withSpan p = do
  start <- M.getOffset
  x <- p
  end <- M.getOffset
  return ((start, end), x)

literal :: Parser SpannedExpr
literal =
  -- fmap (\(p, (l, t)) -> (p,) $ Sy.ELiteral l t) $
  withSpan $
    fmap (uncurry Sy.ELiteral) $
      satisfy isLit
        <&> \(_, tok) -> case tok of
          L.TInt i -> (Sy.LInt i, Sy.TInt)
          L.TDouble d -> (Sy.LDouble d, Sy.TDouble)
          L.TStr s -> (Sy.LStr s, Sy.TStr)
          L.TChar c -> (Sy.LChar c, Sy.TChar)
          L.TBool b -> (Sy.LBool b, Sy.TBool)
          _ -> error "impossible"
  where
    isLit = \case
      L.TInt _ -> True
      L.TDouble _ -> True
      L.TStr _ -> True
      L.TChar _ -> True
      L.TBool _ -> True
      _ -> False

sqbracExpr :: Parser SpannedExpr
sqbracExpr =
  M.try $
    (snd <$> consume L.TLeftBracket)
      *> expr
      <* (snd <$> consume L.TRightBracket)

subscript :: Parser SpannedExpr
subscript = M.try $ do
  e <- ident
  es <- M.some sqbracExpr
  let foldSubscr =
        foldl
          ( \acc ex@(sp, e) ->
              let end = snd sp
               in (((fst . fst) acc, end),) $
                    Sy.ESubscr acc ex
          )
  return $ foldSubscr e es

cast :: Parser SpannedExpr
cast =
  M.try . withSpan $
    consume L.TCast
      *> delimited L.TLeftParen L.TRightParen typee
      >>= \t -> (Sy.ECast <$> expr) <*> pure t

call :: Parser SpannedExpr
call = M.try $ do
  (sp, e) <- withSpan ident
  args <- delimited L.TLeftParen L.TRightParen $ expr `M.sepBy` consume L.TComma
  let end = if not (null args) then (snd . fst) (last args) else snd sp
  return . ((fst sp, end),) $ Sy.ECall e args

isIdent :: Token -> Bool
isIdent = \case
  L.TIdent _ -> True
  _ -> False

ident :: Parser SpannedExpr
ident =
  M.try $
    satisfy isIdent
      <&> \(p, tok) -> case tok of
        L.TIdent i -> (p, Sy.EVariable i)
        _ -> error "impossible"

prim :: Parser SpannedExpr
prim = M.choice [literal, call, subscript, cast, ident]

genOperators ::
  [L.Token] ->
  [SpannedExpr -> SpannedExpr -> Sy.Expr] ->
  Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
genOperators toks ops =
  M.choice $
    zip toks ops <&> \(tok, op) -> consume tok $> op

-- prim (op prim)*
op ::
  Parser SpannedExpr ->
  Parser (SpannedExpr -> SpannedExpr -> Sy.Expr) ->
  Parser SpannedExpr
op base operators =
  base
    >>= \e ->
      M.many
        ( (,)
            <$> operators
            <*> base
        )
        <&> foldl (\acc (op, ex) -> (((fst . fst) acc, (snd . fst) ex),) $ op acc ex) e

factorOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
factorOp =
  genOperators
    [L.TStar, L.TSlash, L.TPercent]
    [Sy.EArith Sy.Mul, Sy.EArith Sy.Div, Sy.EArith Sy.Mod]

factor :: Parser SpannedExpr
factor = op prim factorOp

termOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
termOp =
  genOperators
    [L.TPlus, L.TMinus]
    [Sy.EArith Sy.Add, Sy.EArith Sy.Sub]

term :: Parser SpannedExpr
term = op factor termOp

cmpOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
cmpOp =
  genOperators
    [L.TLessThan, L.TGreaterThan, L.TLessThanOrEqual, L.TGreaterThanOrEqual]
    [Sy.ECmpOp Sy.Lt, Sy.ECmpOp Sy.Gt, Sy.ECmpOp Sy.Le, Sy.ECmpOp Sy.Ge]

cmp :: Parser SpannedExpr
cmp = op term cmpOp

eqOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
eqOp =
  genOperators
    [L.TEqual, L.TNotEqual]
    [Sy.ECmpOp Sy.Eq, Sy.ECmpOp Sy.Ne]

eq :: Parser SpannedExpr
eq = op cmp eqOp

bitOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
bitOp =
  genOperators
    [L.TAmpersand, L.TPipe, L.TCaret, L.TLeftShift, L.TRightShift]
    [Sy.EBitOp Sy.BAnd, Sy.EBitOp Sy.BOr, Sy.EBitOp Sy.BXor, Sy.EBitOp Sy.Shl, Sy.EBitOp Sy.Shr]

bit :: Parser SpannedExpr
bit = op eq bitOp

boolOp :: Parser (SpannedExpr -> SpannedExpr -> Sy.Expr)
boolOp =
  genOperators
    [L.TAnd, L.TOr]
    [Sy.EBoolOp Sy.And, Sy.EBoolOp Sy.Or]

bool :: Parser SpannedExpr
bool = op bit boolOp

assign :: Parser SpannedExpr
assign =
  M.try . withSpan $
    ident
      -- >>= \i -> consume L.TAssign *> expr <&> Sy.EAssign i
      >>= (<&>) (consume L.TAssign *> expr) . Sy.EAssign

make :: Parser SpannedExpr
make =
  M.try . withSpan $
    consume L.TMake
      >> typee
      -- >>= \t -> M.optional expr <&> Sy.EMake t
      >>= (M.optional expr <&>) . Sy.EMake

tuple :: Parser SpannedExpr
tuple =
  M.try . withSpan $
    consume L.TLeftParen
      *> expr `M.sepBy` consume L.TComma
      <* consume L.TRightParen
      <&> Sy.ETuple

chanRead :: Parser SpannedExpr
chanRead =
  M.try . withSpan $
    consume L.TLeftArrow
      *> satisfy isIdent
      <&> \(p, tok) -> case tok of
        L.TIdent i -> Sy.EChanRead i
        _ -> error "impossible"

list :: Parser SpannedExpr
list =
  M.try . withSpan $
    consume L.TLeftBracket
      *> expr `M.sepBy` consume L.TComma
      <* consume L.TRightBracket
      <&> Sy.EList

expr :: Parser (Sy.Spanned Sy.Expr)
expr = M.choice [make, tuple, chanRead, list, bool]
