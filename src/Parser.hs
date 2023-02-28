module Parser (pFile, runParser, errorBundlePretty, ParseErrorBundle (..)) where

import Control.Monad.Combinators.Expr
import Data.Char
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Ast (Expr)
import qualified Ast
import Control.Monad
import Data.Maybe (fromMaybe, isJust)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

keyword :: Text -> Parser ()
keyword = (keywords M.!)

keywords :: Map Text (Parser ())
keywords = M.fromList (zip kws (map kw kws))
 where
  kw w = void $ lexeme $ try (string w <* notFollowedBy alphaNumChar)
  kws =
    [ "let"
    , "in"
    , "true"
    , "false"
    , "if"
    , "then"
    , "else"
    , "or"
    , "and"
    , "not"
    ]

identifier :: Parser Text
identifier = try $ lexeme do
  kw <- optional (choice $ M.elems keywords)
  when (isJust kw) $ fail "keyword"
  initChars <- satisfy ((||) <$> isAlpha <*> (`T.elem` "_"))
  bodyChars <- takeWhileP Nothing ((||) <$> isAlphaNum <*> (`T.elem` "_"))
  return (T.cons initChars bodyChars) <?> "identifier"

numText :: Parser Text
numText = do
  initDigits <- satisfy (`T.elem` "0123456789")
  bodyDigits <- takeWhileP Nothing (`T.elem` "_0123456789")
  return $ T.cons initDigits (T.filter (/= '_') bodyDigits)

pNum :: Parser Expr
pNum =
  lexeme
    ( try do
        natPart <- fromMaybe "0" <$> optional numText
        void $ char '.'
        fracPart <- numText
        return $ Ast.Literal $ Ast.Real $ mkNat natPart + mkFrac fracPart
        <|> Ast.Literal . Ast.Int . mkNat <$> numText
    )
 where
  mkNat :: Num a => Text -> a
  mkNat = T.foldl natStep 0
  natStep acc c = acc * 10 + fromIntegral (fromEnum c - fromEnum '0')
  mkFrac = (/ 10) . T.foldr fracStep 0
  fracStep c acc = acc / 10 + fromIntegral (fromEnum c - fromEnum '0')

pIdent :: Parser Expr
pIdent = Ast.Ident <$> lexeme identifier

pString :: Parser Expr
pString = Ast.Literal . Ast.Str <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

pBool :: Parser Expr
pBool = Ast.Literal . Ast.Bool <$> choice [True <$ keyword "true", False <$ keyword "false"]

pLet :: Parser Expr
pLet = do
  keyword "let"
  letEnv <- many pBind
  keyword "in"
  Ast.Let (M.fromList letEnv) <$> pExpr
 where
  pBind = do
    name <- identifier
    void $ symbol "="
    body <- pExpr
    void $ symbol ";"
    return (name, body)

pCond :: Parser Expr
pCond = do
  keyword "if"
  condExpr <- pExpr
  keyword "then"
  thenExpr <- pExpr
  keyword "else"
  Ast.Cond condExpr thenExpr <$> pExpr

pDef :: Parser Expr
pDef = do
  args <- between (symbol "|") (symbol "|") (many identifier)
  Ast.Def args <$> pExpr

pAtom :: Parser Expr
pAtom =
  choice
    [ pLet
    , pNum
    , pIdent
    , pString
    , pBool
    , pDef
    , pCond
    , Ast.Group <$> between (symbol "(") (symbol ")") pExpr
    ]

preSym, preKw :: Text -> (Expr -> Expr) -> Operator Parser Expr
preSym name f = Prefix (f <$ symbol name)
preKw name f = Prefix (f <$ keyword name)

binSym, binKw :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binSym name f = InfixL (f <$ symbol name)
binKw name f = InfixL (f <$ keyword name)

pCall :: Operator Parser Expr
pCall = InfixL (return mkCall)
 where
  mkCall (Ast.Call expr args) arg = Ast.Call expr (args ++ [arg])
  mkCall expr arg = Ast.Call expr [arg]

pExpr :: Parser Expr
pExpr =
  fixup
    <$> makeExprParser
      pAtom
      [
        [ preSym "-" (Ast.Unary Ast.Neg)
        ]
      ,
        [ binSym "*" (Ast.Binary Ast.Mul)
        , binSym "/" (Ast.Binary Ast.Div)
        ]
      ,
        [ binSym "+" (Ast.Binary Ast.Add)
        , binSym "-" (Ast.Binary Ast.Sub)
        ]
      ,
        [ pCall
        ]
      ,
        [ binSym "==" (Ast.Binary Ast.Eq)
        , binSym "!=" (Ast.Binary Ast.NEq)
        , binSym "<" (Ast.Binary Ast.Lt)
        , binSym "<=" (Ast.Binary Ast.EqLt)
        , binSym ">" (Ast.Binary Ast.Gt)
        , binSym ">=" (Ast.Binary Ast.EqGt)
        ]
      ,
        [ preKw "not" (Ast.Unary Ast.Not)
        ]
      ,
        [ binKw "or" (Ast.Binary Ast.Or)
        , binKw "and" (Ast.Binary Ast.And)
        ]
      ]

pFile :: Parser Expr
pFile = pExpr <* eof

fixup :: Expr -> Expr
fixup (Ast.Group expr) = fixup expr
fixup (Ast.Unary op expr) = Ast.Unary op (fixup expr)
fixup (Ast.Binary op lhs rhs) = Ast.Binary op (fixup lhs) (fixup rhs)
fixup (Ast.Call expr args) = Ast.Call (fixup expr) (map fixup args)
fixup (Ast.Let letEnv body) = Ast.Let (M.map fixup letEnv) (fixup body)
fixup (Ast.Def args body) = Ast.Def args (fixup body)
fixup expr = expr -- Lietrals and Idents stay the same
