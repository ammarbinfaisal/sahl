{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.Char (Char, isAlphaNum)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set, toList)
import Data.Text (Text, intercalate, pack, unpack)
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

satisfy :: (Token -> Bool) -> Parser ((Int, Int), Token)
satisfy f = M.satisfy (\(p, tok) -> f tok)

consume :: Token -> Parser (Int, Int)
consume tok = satisfy (== tok) <&> fst

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
tuple :: Parser Sy.Type
tuple = M.try $ delimited L.TLeftParen L.TRightParen (typee `M.sepBy` consume L.TComma) <&> Sy.TTuple

-- map<type, type>
mapty :: Parser Sy.Type
mapty =
  M.try $
    consume
      (L.TIdent "map")
      *> delimited L.TLessThan L.TGreaterThan ((,) <$> typee <*> typee)
      <&> uncurry Sy.TMap

typee :: Parser Sy.Type
typee = M.choice [simpleType, listty, chanty, tuple, mapty]

type SpannedExpr = Sy.Spanned Sy.Expr

withSpan :: (a -> a) -> (Int, Int) -> a -> ((Int, Int), a)
withSpan f p = (p,) . f

literal :: Parser SpannedExpr
literal =
  -- fmap (\(p, (l, t)) -> (p,) $ Sy.ELiteral l t) $
  fmap (second $ uncurry Sy.ELiteral) $
    satisfy isLit
      <&> \(p, tok) -> (p,) $ case tok of
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
sqbracExpr = M.try $ do
  sp1 <- consume L.TLeftBracket
  e <- expr
  sp2 <- consume L.TRightBracket
  let span = (fst sp1, snd sp2)
  return (span, snd e)

subscript :: Parser SpannedExpr
subscript = M.try $ do
  e <- expr
  es <- M.some sqbracExpr
  let start = fst $ fst e
  let end = fst . fst $ last es
  let span = (start, end)
  let foldSubscr =
        foldl
          ( \acc ex@(sp, e) ->
              let end = snd sp
               in ((start, end),) $
                    Sy.ESubscr acc ex
          )
  return $ foldSubscr e es

cast :: Parser SpannedExpr
cast = M.try $ do
  sp1 <- consume L.TLeftParen
  e <- expr
  consume L.TColon
  t <- typee
  sp2 <- consume L.TRightParen
  let span = (fst sp1, snd sp2)
  return $ (span,) $ Sy.ECast e t

expr :: Parser (Sy.Spanned Sy.Expr)
expr = M.choice [cast, literal, subscript]
