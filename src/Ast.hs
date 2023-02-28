module Ast where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Expr
  = Literal Lit
  | Ident Text
  | Group Expr
  | Unary Op Expr
  | Binary Op Expr Expr
  | Call Expr [Expr]
  | Let (Map Text Expr) Expr
  | Cond Expr Expr Expr
  | Def [Text] Expr
  deriving (Eq)

data Lit
  = Int Integer
  | Real Double
  | Str Text
  | Bool Bool
  deriving (Eq)

data Op
  = Neg
  | Not
  | And
  | Or
  | Eq
  | NEq
  | Lt
  | Gt
  | EqLt
  | EqGt
  | Add
  | Sub
  | Mul
  | Div
  deriving (Eq)

instance Show Expr where
  show (Literal lit) = show lit
  show (Ident i) = "i!(" ++ T.unpack i ++ ")"
  show (Group expr) = "(" ++ show expr ++ ")"
  show (Unary op expr) = "(" ++ unwords [show op, show expr] ++ ")"
  show (Binary op lhs rhs) = "(" ++ unwords [show lhs, show op, show rhs] ++ ")"
  show (Call expr args) = "(" ++ show expr ++ " " ++ unwords (map show args) ++ ")"
  show (Let letEnv body) =
    "(let "
      ++ unwords
        [T.unpack name ++ " = " ++ show val ++ ";" | (name, val) <- M.toList letEnv]
      ++ " in "
      ++ show body
      ++ ")"
  show (Cond cond true false) =
    "(if "
      ++ show cond
      ++ " then "
      ++ show true
      ++ " else "
      ++ show false
      ++ ")"
  show (Def args body) =
    "(|"
      ++ unwords (map T.unpack args)
      ++ "| "
      ++ show body
      ++ ")"

instance Show Lit where
  show (Int i) = show i
  show (Real r) = show r
  show (Str s) = T.unpack s
  show (Bool b) = show b

instance Show Op where
  show Neg = "-"
  show Not = "not"
  show And = "and"
  show Or = "or"
  show Eq = "=="
  show NEq = "!="
  show Lt = "<"
  show Gt = ">"
  show EqLt = "<="
  show EqGt = ">="
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
