module Syntax where

import Data.Int (Int64)
import Data.Text (Text)

data Type
  = TInt
  | TStr
  | TChar
  | TBool
  | TVoid
  | TDouble
  | TAny
  | TList Type
  | TChan Type
  | TTuple [Type]
  | TMap Type Type
  | TRange
  | TRef Type
  | TCustom String
  | TVariant [(String, Type)]
  deriving (Show, Eq)

data Lit
  = LInt Int64
  | LChar Char
  | LBool Bool
  | LStr Text
  | LDouble Double
  deriving (Show, Eq)

data ArithOp = Add | Sub | Mul | Div | Mod deriving (Show, Eq)

data BoolOp = And | Or deriving (Show, Eq)

data CmpOp = Eq | Ne | Lt | Gt | Le | Ge deriving (Show, Eq)

data BitOp = BAnd | BOr | BXor | Shl | Shr deriving (Show, Eq)

data Expr
  = ELiteral Lit Type
  | EVariable Text
  | ENeg (Spanned Expr)
  | ENot (Spanned Expr)
  | EBitNot (Spanned Expr)
  | ERange (Spanned Expr) (Spanned Expr) Bool
  | EArith ArithOp (Spanned Expr) (Spanned Expr)
  | EBoolOp BoolOp (Spanned Expr) (Spanned Expr)
  | ECmpOp CmpOp (Spanned Expr) (Spanned Expr)
  | EBitOp BitOp (Spanned Expr) (Spanned Expr)
  | ECall (Spanned Expr) [Spanned Expr]
  | ESubscr (Spanned Expr) (Spanned Expr)
  | EAssign (Spanned Expr) (Spanned Expr)
  | EMake Type (Maybe (Spanned Expr))
  | ETuple [Spanned Expr]
  | EChanRead Text
  | EList [Spanned Expr]
  | ECast (Spanned Expr) Type
  | ERef (Spanned Expr)
  | EDeref (Spanned Expr)
  | EIs (Spanned Expr) (Maybe Int) Type
  deriving (Show, Eq)

data TypedExpr = TypedExpr {expr :: Expr, ty :: Type} deriving (Show, Eq)

type Spanned a = ((Int, Int), a)

data Stmt
  = SExpr (Spanned Expr)
  | SDecl (Spanned Expr) (Spanned Expr)
  | SFor String (Spanned Expr) [Spanned Stmt]
  | SWhile (Spanned Expr) [Spanned Stmt]
  | SIfElse (Spanned Expr) [Spanned Stmt] (Maybe [Spanned Stmt])
  | SReturn (Maybe (Spanned Expr))
  | SCoroutine (Spanned Expr)
  | SChanWrite String (Spanned Expr)
  | SBlock [Spanned Stmt]
  | SContinue
  | SBreak
  deriving (Show, Eq)

data Param = Param {pname :: String, ptype :: Type} deriving (Show, Eq)

data Func = Func {fname :: String, fargs :: [Param], fbody :: [Spanned Stmt], fretty :: Type, fexterned :: Bool}
  deriving (Show, Eq)

data TopLevel = TFunc Func | TTypedef String Type | TConst String (Spanned Expr) deriving (Show, Eq)

newtype Program = Program {topLevels :: [TopLevel]} deriving (Show, Eq)
