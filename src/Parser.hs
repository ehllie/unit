module Parser (pExpr, runParser, errorBundlePretty, ParseErrorBundle (..)) where

import Control.Monad.Combinators.Expr
import Data.Char
import Data.Map.Lazy (Map, fromList)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Ast
import Control.Monad
import Data.Maybe (fromMaybe, isJust)
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

keyword :: Text -> Parser ()
keyword = (keywords M.!)

keywords :: Map Text (Parser ())
keywords = fromList (zip kws (map kw kws))
 where
  kw w = void $ lexeme $ try (string w <* notFollowedBy alphaNumChar)
  kws =
    [ "let"
    , "in"
    , "true"
    , "false"
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

pNum :: Parser Ast.Expr
pNum =
  lexeme
    ( try do
        natPart <- fromMaybe "0" <$> optional numText
        void $ char '.'
        fracPart <- numText
        return $ Ast.Real $ mkNat natPart + mkFrac fracPart
        <|> Ast.Int . mkNat <$> numText
    )
 where
  mkNat :: Num a => Text -> a
  mkNat = T.foldl natStep 0
  natStep acc c = acc * 10 + fromIntegral (fromEnum c - fromEnum '0')
  mkFrac = (/ 10) . T.foldr fracStep 0
  fracStep c acc = acc / 10 + fromIntegral (fromEnum c - fromEnum '0')

pIdent :: Parser Ast.Expr
pIdent = Ast.Ident <$> lexeme identifier

pString :: Parser Ast.Expr
pString = Ast.Str <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

pBool :: Parser Ast.Expr
pBool = Ast.Bool <$> choice [True <$ keyword "true", False <$ keyword "false"]

pLet :: Parser Ast.Expr
pLet = do
  keyword "let"
  letEnv <- many pBind
  keyword "in"
  Ast.Let (fromList letEnv) <$> pExpr
 where
  pBind = do
    name <- identifier
    void $ symbol "="
    body <- pExpr
    void $ symbol ";"
    return (name, body)

pAtom :: Parser Ast.Expr
pAtom =
  choice
    [ pLet
    , pNum
    , pIdent
    , pString
    , pBool
    ]

pExpr :: Parser Ast.Expr
pExpr = makeExprParser pAtom []
