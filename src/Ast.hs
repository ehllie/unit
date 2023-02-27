module Ast where

import Data.Map.Lazy (Map)
import Data.Text.Lazy (Text)

data Expr
  = Int Integer
  | Real Double
  | Str Text
  | Bool Bool
  | Ident Text
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Call Expr Expr
  | Let (Map Text Expr) Expr
  | Def [String] Expr
  deriving (Show)
