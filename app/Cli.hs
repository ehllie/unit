module Cli (Input (..), Args (..), execArgsParser) where

import Options.Applicative (
  Alternative ((<|>)),
  Parser,
  execParser,
  flag,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  short,
  strArgument,
  strOption,
  (<**>),
 )

data Input = File FilePath | Expr String
  deriving (Show)

data Args = Args {input :: Input, ast :: Bool}
  deriving (Show)

parseInput :: Parser Input
parseInput = file <|> expr
 where
  file = File <$> strArgument (metavar "PATH" <> help "Path to the input file")
  expr = Expr <$> strOption (long "expr" <> short 'e' <> metavar "EXPR" <> help "Expression to evaluate")

parseArgs :: Parser Args
parseArgs =
  Args
    <$> parseInput
    <*> flag False True (long "ast" <> short 'a' <> help "Print the AST and exit")

execArgsParser :: IO Args
execArgsParser =
  execParser $
    info
      (parseArgs <**> helper)
      (fullDesc <> progDesc "A small personal language project")
