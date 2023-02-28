module Eval (eval, runEval) where

import Control.Monad.Except
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy (Text, unpack)

import Ast (Expr)
import qualified Ast

data Err
  = TypeMismatch
  | UnsetEnv {name :: Text}
  deriving (Show)

type Env = Map Text Object

type EvalRes = ReaderT Env (Except Err)

data Object
  = PrimInt Integer
  | PrimReal Double
  | PrimStr Text
  | PrimBool Bool
  | Function {params :: [Text], body :: Expr}
  | Thunk {expr :: Expr, env :: Env}
  deriving (Eq)

instance Show Object where
  show (PrimInt i) = show i
  show (PrimReal r) = show r
  show (PrimStr s) = unpack s
  show (PrimBool b) = show b
  show (Function{}) = "<function>"
  show (Thunk{}) = "<thunk>"

strict :: Object -> EvalRes Object
strict (Thunk expr env) = local (env `M.union`) $ eval expr >>= strict
strict obj = return obj

freeVars :: Expr -> Set Text
freeVars (Ast.Literal _) = S.empty
freeVars (Ast.Ident name) = S.singleton name
freeVars (Ast.Group expr) = freeVars expr
freeVars (Ast.Unary _ expr) = freeVars expr
freeVars (Ast.Binary _ lhsExpr rhsExpr) = freeVars lhsExpr `S.union` freeVars rhsExpr
freeVars (Ast.Let letEnv body) = S.unions (freeVars body : [freeVars expr | expr <- M.elems letEnv]) `S.difference` S.fromList (M.keys letEnv)
freeVars (Ast.Cond condExp thenExp elseExp) = freeVars condExp `S.union` freeVars thenExp `S.union` freeVars elseExp
freeVars (Ast.Call funcExpr args) = freeVars funcExpr `S.union` S.unions (map freeVars args)
freeVars (Ast.Def params body) = freeVars body `S.difference` S.fromList params

mkThunk :: Expr -> EvalRes Object
mkThunk expr = do
  env <- asks (M.filterWithKey (\k _ -> k `S.member` freeVars expr))
  return Thunk{expr, env}

eval :: Expr -> EvalRes Object
eval (Ast.Literal (Ast.Int lit)) = return $ PrimInt lit
eval (Ast.Literal (Ast.Real lit)) = return $ PrimReal lit
eval (Ast.Literal (Ast.Str lit)) = return $ PrimStr lit
eval (Ast.Literal (Ast.Bool lit)) = return $ PrimBool lit
eval (Ast.Ident name) = do
  val <- asks (M.lookup name)
  maybe (throwError UnsetEnv{name}) return val
eval (Ast.Group expr) = eval expr
eval (Ast.Unary op expr) = do
  obj <- strict =<< eval expr
  case (op, obj) of
    (Ast.Neg, PrimInt i) -> return $ PrimInt (-i)
    (Ast.Neg, PrimReal r) -> return $ PrimReal (-r)
    (Ast.Not, PrimBool b) -> return $ PrimBool (not b)
    _ -> throwError TypeMismatch
eval (Ast.Binary op lhsExpr rhsExpr) = do
  lhs <- strict =<< eval lhsExpr
  rhs <- strict =<< eval rhsExpr
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
eval (Ast.Let letEnv body) = do
  bindings <- mapM mkThunk letEnv
  local (bindings `M.union`) $ eval body
eval (Ast.Cond condExp thenExp elseExp) = do
  cond <- strict =<< eval condExp
  case cond of
    PrimBool True -> eval thenExp
    PrimBool False -> eval elseExp
    _ -> throwError TypeMismatch
eval (Ast.Call funcExpr args) = do
  func <- strict =<< eval funcExpr
  case func of
    Function{params, body} -> do
      when (length params /= length args) $ throwError TypeMismatch
      argClosures <- mapM mkThunk args
      local (M.fromList (zip params argClosures) `M.union`) $ eval body
    _ -> throwError TypeMismatch
eval (Ast.Def params body) = return $ Function{params, body}

runEval :: Expr -> Either Err Object
runEval expr = runExcept $ runReaderT (strict =<< eval expr) M.empty
