module Eval (eval, runEval) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text, unpack)
import Control.Monad.RWS.Lazy

import Ast (Expr)
import qualified Ast

data Err
  = TypeMismatch
  | UnsetEnv {name :: Text}
  deriving (Show)

type Env = Map Text Expr

type EvalRes = ReaderT Env (Except Err)

data Object
  = PrimInt Integer
  | PrimReal Double
  | PrimStr Text
  | PrimBool Bool
  | Function {params :: [Text], body :: Expr}
  deriving (Eq)

instance Show Object where
  show (PrimInt i) = show i
  show (PrimReal r) = show r
  show (PrimStr s) = unpack s
  show (PrimBool b) = show b
  show (Function{}) = "<function>"

-- expect :: Text -> EvalRes Expr
-- expect name = do
--   val <- asks (M.lookup name)
--   maybe (throwError UnsetEnv{name}) return val

eval :: Expr -> EvalRes Object
eval (Ast.Literal (Ast.Int lit)) = return $ PrimInt lit
eval (Ast.Literal (Ast.Real lit)) = return $ PrimReal lit
eval (Ast.Literal (Ast.Str lit)) = return $ PrimStr lit
eval (Ast.Literal (Ast.Bool lit)) = return $ PrimBool lit
eval (Ast.Ident name) = do
  val <- asks (M.lookup name)
  maybe (throwError UnsetEnv{name}) eval val
eval (Ast.Group expr) = eval expr
eval (Ast.Unary op expr) = do
  obj <- eval expr
  case (op, obj) of
    (Ast.Neg, PrimInt i) -> return $ PrimInt (-i)
    (Ast.Neg, PrimReal r) -> return $ PrimReal (-r)
    (Ast.Not, PrimBool b) -> return $ PrimBool (not b)
    _ -> throwError TypeMismatch
eval (Ast.Binary op lhsExpr rhsExpr) = do
  lhs <- eval lhsExpr
  rhs <- eval rhsExpr
  case (op, lhs, rhs) of
    (Ast.Add, PrimInt lhsI, PrimInt rhsI) -> return $ PrimInt (lhsI + rhsI)
    (Ast.Add, PrimReal lhsR, PrimReal rhsR) -> return $ PrimReal (lhsR + rhsR)
    (Ast.Sub, PrimInt lhsI, PrimInt rhsI) -> return $ PrimInt (lhsI - rhsI)
    (Ast.Sub, PrimReal lhsR, PrimReal rhsR) -> return $ PrimReal (lhsR - rhsR)
    (Ast.Mul, PrimInt lhsI, PrimInt rhsI) -> return $ PrimInt (lhsI * rhsI)
    (Ast.Mul, PrimReal lhsR, PrimReal rhsR) -> return $ PrimReal (lhsR * rhsR)
    (Ast.Div, PrimInt lhsI, PrimInt rhsI) -> return $ PrimInt (lhsI `div` rhsI)
    (Ast.Div, PrimReal lhsR, PrimReal rhsR) -> return $ PrimReal (lhsR / rhsR)
    (Ast.And, PrimBool lhsB, PrimBool rhsB) -> return $ PrimBool (lhsB && rhsB)
    (Ast.Or, PrimBool lhsB, PrimBool rhsB) -> return $ PrimBool (lhsB || rhsB)
    (Ast.Lt, PrimInt lhsI, PrimInt rhsI) -> return $ PrimBool (lhsI < rhsI)
    (Ast.Lt, PrimReal lhsR, PrimReal rhsR) -> return $ PrimBool (lhsR < rhsR)
    (Ast.Gt, PrimInt lhsI, PrimInt rhsI) -> return $ PrimBool (lhsI > rhsI)
    (Ast.Gt, PrimReal lhsR, PrimReal rhsR) -> return $ PrimBool (lhsR > rhsR)
    (Ast.EqLt, PrimInt lhsI, PrimInt rhsI) -> return $ PrimBool (lhsI <= rhsI)
    (Ast.EqLt, PrimReal lhsR, PrimReal rhsR) -> return $ PrimBool (lhsR <= rhsR)
    (Ast.EqGt, PrimInt lhsI, PrimInt rhsI) -> return $ PrimBool (lhsI >= rhsI)
    (Ast.EqGt, PrimReal lhsR, PrimReal rhsR) -> return $ PrimBool (lhsR >= rhsR)
    (Ast.Eq, _, _) -> return $ PrimBool (lhs == rhs)
    (Ast.NEq, _, _) -> return $ PrimBool (lhs /= rhs)
    _ -> throwError TypeMismatch
eval (Ast.Let letEnv body) = local (letEnv `M.union`) $ eval body
eval (Ast.Cond condExp thenExp elseExp) = do
  cond <- eval condExp
  case cond of
    PrimBool True -> eval thenExp
    PrimBool False -> eval elseExp
    _ -> throwError TypeMismatch
eval (Ast.Call funcExpr args) = do
  func <- eval funcExpr
  case func of
    Function{params, body} -> do
      when (length params /= length args) $ throwError TypeMismatch
      local (M.fromList (zip params args) `M.union`) $ eval body
    _ -> throwError TypeMismatch
eval (Ast.Def params body) = return $ Function{params, body}

runEval :: Expr -> Either Err Object
runEval expr = runExcept $ runReaderT (eval expr) M.empty
