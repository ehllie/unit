module Main where

import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as LIO

import Cli
import Parser

main :: IO ()
main = do
  Args{input} <- execArgsParser
  (content, name) <- case input of
    File path -> (,path) <$> LIO.readFile path
    Expr expr -> return (pack expr, "stdin")
  putStrLn
    . either errorBundlePretty show
    $ runParser pExpr name content
