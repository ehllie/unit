module Main where

import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as LIO

import Cli
import Eval
import Parser

main :: IO ()
main = do
  Args{input, ast} <- execArgsParser
  (content, name) <- case input of
    File path -> (,path) <$> LIO.readFile path
    Expr expr -> return (pack expr, "stdin")
  case runParser pFile name content of
    Right expr ->
      if ast
        then print expr
        else case runEval expr of
          (ret, evalLog) -> do
            mapM_ putStrLn evalLog
            either print print ret
    Left err -> putStrLn $ errorBundlePretty err
  return ()
